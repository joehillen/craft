{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Craft.Types
( module Craft.Types
, module Craft.Error
, module Craft.File.Mode
, module Path
)
where

import           Control.Lens
import           Control.Monad.Catch        (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Logger       (LoggingT, MonadLogger, logDebugN,
                                             monadLoggerLog)
import           Control.Monad.Reader       (MonadReader, ReaderT, runReaderT)
import qualified Control.Monad.Trans.Class  as Trans
import           Control.Monad.Trans.Free   (FreeT, MonadFree, iterT)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as B8
import           Data.ByteString.Lens       (unpackedChars)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (isNothing)
import qualified Data.Text                  as T
import           Data.Versions              (parseV)
import           Formatting
import           Language.Haskell.TH.Syntax (Exp, Q)
import           Path                       hiding (File)
import qualified Path
import           Prelude                    hiding (FilePath)
import qualified Prelude
import           System.Process

import           Craft.Error
import           Craft.File.Mode
import           Craft.Internal.Helpers


-- | FileP is an alias because 'Path.File' collides with 'Craft.File'.
-- The alias was not named FilePath because it would conflict with Prelude.
type FileP = Path.File

data CraftEnv
  = CraftEnv
    { _craftPackageManager :: PackageManager
    , _craftExecEnv        :: ExecEnv
    , _craftExecCWD        :: Path Abs Dir
    }


craftEnv :: PackageManager -> CraftEnv
craftEnv pm =
  CraftEnv
  { _craftPackageManager = pm
  , _craftExecEnv        = Map.fromList [("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")]
  , _craftExecCWD        = $(mkAbsDir "/")
  }


newtype Craft a = Craft { unCraft :: ReaderT CraftEnv (FreeT CraftDSL (LoggingT IO)) a }
  deriving ( Functor, Monad, MonadIO, Applicative, MonadReader CraftEnv
           , MonadFree CraftDSL, MonadThrow, MonadCatch, MonadLogger)


instance (MonadLogger m, Functor f) => MonadLogger (FreeT f m) where
  monadLoggerLog a b c d = Trans.lift $ monadLoggerLog a b c d


interpretCraft :: CraftEnv -> (CraftDSL (LoggingT IO a) -> LoggingT IO a) -> Craft a -> LoggingT IO a
interpretCraft ce interpreter = iterT interpreter . flip runReaderT ce . unCraft


data CraftRunner = CraftRunner
  { runExec       :: CraftEnv -> Command -> Args -> LoggingT IO ExecResult
  , runExec_      :: CraftEnv -> Command -> Args -> LoggingT IO ()
  , runFileRead   :: Path Abs FileP -> LoggingT IO ByteString
  , runFileWrite  :: Path Abs FileP -> ByteString -> LoggingT IO ()
  , runSourceFile :: Prelude.FilePath -> Path Abs FileP -> LoggingT IO ()
  }


runCraft :: CraftRunner -> CraftEnv -> Craft a -> LoggingT IO a
runCraft runner ce dsl = do
  iterT interpreter $ flip runReaderT ce $ unCraft dsl
 where
   interpreter (Exec ce' cmd args next) = do
     logDebugN $ sformat ("Exec "%string%" "%string) cmd (unwords args)
     (runExec runner) ce' cmd args >>= next
   interpreter (Exec_ ce' cmd args next) = do
     logDebugN $ sformat ("Exec_ "%string%" "%string) cmd (unwords args)
     (runExec_ runner) ce' cmd args >> next
   interpreter (FileRead fp next) = do
     logDebugN $ sformat ("FileRead "%shown) fp
     (runFileRead runner) fp >>= next
   interpreter (FileWrite fp content next) = do
     logDebugN $ sformat ("FileWrite "%shown) fp
     (runFileWrite runner) fp content >> next
   interpreter (SourceFile sourcer dest next) = do
     src <- Trans.lift sourcer
     logDebugN $ sformat ("SourceFile "%string%" "%shown) src dest
     (runSourceFile runner) src dest >> next


type StdOut  = String
type StdErr  = String
type Args    = [String]
type Command = String


data SuccResult
  = SuccResult
    { _stdout   :: StdOut
    , _stderr   :: StdErr
    , _succProc :: CreateProcess
    }


data FailResult
  = FailResult
    { _exitcode   :: Int
    , _failStdout :: StdOut
    , _failStderr :: StdErr
    , _failProc   :: CreateProcess
    }


data ExecResult
  = ExecFail FailResult
  | ExecSucc SuccResult


isSuccess :: ExecResult -> Bool
isSuccess (ExecSucc _) = True
isSuccess (ExecFail _) = False


isFailure :: ExecResult -> Bool
isFailure = not . isSuccess


errorOnFail :: Q Exp
errorOnFail = [|
  \case
    ExecSucc r -> return r
    ExecFail r -> $craftError $ show r|]


-- | Try to get STDOUT from a process.
-- If the command exits with an error code, throw a CraftError.
stdoutOrError :: Q Exp
stdoutOrError = [|
  \case
    ExecSucc r -> return $ _stdout r
    ExecFail r -> $craftError $ show r|]


type ExecEnv = Map String String
type CWD = Path Abs FileP
type PackageName = String

newtype UserName = UserName String
                   deriving (Eq, Ord)
newtype UserID = UserID Int
                 deriving (Eq, Show, Ord)
newtype GroupName = GroupName String
                    deriving (Eq, Ord)
newtype GroupID = GroupID Int
                  deriving (Eq, Show, Ord)

instance Show GroupName where
  show (GroupName n) = n

instance Show UserName where
  show (UserName n) = n

instance ToArg UserID where
  toArg arg (UserID n) = [arg, show n]

instance ToArg GroupID where
  toArg arg (GroupID n) = [arg, show n]



class Eq (FileLikePath a) => FileLike a where
  type FileLikePath a :: *
  path    :: Lens' a (FileLikePath a)
  mode    :: Lens' a Mode
  ownerID :: Lens' a UserID
  groupID :: Lens' a GroupID


data File
  = File
  { _filePath    :: Path Abs FileP
  , _fileMode    :: Mode
  , _fileOwnerID :: UserID
  , _fileGroupID :: GroupID
  , _fileContent :: Maybe ByteString
  }


file :: Path Abs FileP -> File
file fp =
  File
  { _filePath    = fp
  , _fileMode    = Mode RW R R
  , _fileOwnerID = UserID 0
  , _fileGroupID = GroupID 0
  , _fileContent = Nothing
  }



data Directory
  = Directory
  { _directoryPath    :: Path Abs Dir
  , _directoryMode    :: Mode
  , _directoryOwnerID :: UserID
  , _directoryGroupID :: GroupID
  }
  deriving (Show, Eq)


directory :: Path Abs Dir -> Directory
directory dp =
  Directory
  { _directoryPath    = dp
  , _directoryMode    = Mode RWX RX RX
  , _directoryOwnerID = UserID 0
  , _directoryGroupID = GroupID 0
  }


data Version
  = Version String
  | AnyVersion
  | Latest
  deriving (Show)


-- Note: This may or may not make sense.
-- Open to suggestions if any of this seems incorrect.
instance Eq Version where
  (==) AnyVersion  _           = True
  (==) _           AnyVersion  = True
  (==) Latest      Latest      = True
  (==) Latest      (Version _) = False
  (==) (Version _) Latest      = False
  (==) (Version a) (Version b) = a == b


data Package
  = Package
    { _pkgName    :: PackageName
    , _pkgVersion :: Version
    }
  deriving (Eq, Show)


data PackageManager
 = PackageManager
   { _pmGetter      :: PackageName -> Craft (Maybe Package)
   , _pmInstaller   :: Package     -> Craft ()
   , _pmUpgrader    :: Package     -> Craft ()
   , _pmUninstaller :: Package     -> Craft ()
   }


noPackageManager :: PackageManager
noPackageManager =
  let err _ = $craftError "No Package Manager" in
  PackageManager
  { _pmGetter         = err
  , _pmInstaller      = err
  , _pmUpgrader       = err
  , _pmUninstaller    = err
  }


data CraftDSL next
  = Exec  CraftEnv Command Args (ExecResult -> next)
  | Exec_ CraftEnv Command Args next
  | FileRead (Path Abs FileP) (ByteString -> next)
  | FileWrite (Path Abs FileP) ByteString next
  | SourceFile (IO Prelude.FilePath) (Path Abs FileP) next
 deriving Functor


data CraftRunDSL next
  = CraftRunDSL (CraftDSL next)


makeLenses ''PackageManager
makeLenses ''CraftEnv
makeLenses ''Package
makePrisms ''Version
makeLenses ''FailResult
makeLenses ''SuccResult
makeLenses ''File
makeLenses ''Directory



strContent :: Lens' File String
strContent = lens (view $ fileContent . _Just . unpackedChars)
                  (\f s -> f & fileContent .~ Just (B8.pack s))

instance Eq File where
  (==) a b = (a ^. filePath == b ^. filePath)
          && (a ^. fileMode == b ^. fileMode)
          && (a ^. fileOwnerID == b ^. fileOwnerID)
          && (a ^. fileGroupID == b ^. fileGroupID)
          && (  isNothing (a ^. fileContent)
             || isNothing (b ^. fileContent)
             || (a ^. fileContent == b ^. fileContent))


instance Show File where
  show f = "File { _filePath = " ++ show (f ^. filePath) ++
                ", _fileMode = " ++ show (f ^. fileMode) ++
                ", _fileOwnerID = " ++ show (f ^. fileOwnerID) ++
                ", _fileGroupID = " ++ show (f ^. fileGroupID) ++
                ", _fileContent = " ++ showContent (f ^. fileContent) ++
               " }"
    where
      maxlen = 30
      showContent Nothing = "Nothing"
      showContent (Just c)
        | BS.length c > maxlen = "Just " ++ show (BS.take maxlen c) ++ "..."
        | otherwise            = "Just " ++ show c





instance FileLike File where
  type FileLikePath File = Path Abs FileP
  path = filePath
  mode = fileMode
  ownerID = fileOwnerID
  groupID = fileGroupID


instance FileLike Directory where
  type FileLikePath Directory = Path Abs Dir
  path = directoryPath
  mode = directoryMode
  ownerID = directoryOwnerID
  groupID = directoryGroupID


data User
  = User
    { _userName         :: UserName
    , _uid              :: UserID
    , _userComment      :: String
    , _userGroup        :: Group
    , _userGroups       :: [GroupName]
    , _userHome         :: Path Abs Dir
    , _userPasswordHash :: String
    --, _salt         :: String
    --, _locked       :: Bool
    , _userShell        :: Path Abs FileP
    --, system       :: Bool
    }
 deriving (Eq, Show)


data Group
  = Group
    { _groupName    :: GroupName
    , _gid          :: GroupID
    , _groupMembers :: [UserName]
    }
  deriving (Eq, Show)


makeLenses ''User
makeLenses ''Group




owner :: FileLike a => Setter a a () User
owner = sets (\functor filelike -> doit filelike (functor ()))
 where doit filelike o = filelike & ownerID .~ (o ^. uid)


group :: FileLike a => Setter a a () Group
group = sets (\functor filelike -> doit filelike (functor ()))
 where doit filelike g = filelike & groupID .~ (g ^. gid)


ownerAndGroup :: FileLike a => Setter a a () User
ownerAndGroup = sets (\functor filelike -> doit filelike (functor ()))
 where doit filelike u = filelike & owner .~ u
                                  & group .~ (u ^. userGroup)


execResultProc :: ExecResult -> CreateProcess
execResultProc (ExecFail failr) = failr ^. failProc
execResultProc (ExecSucc succr) = succr ^. succProc


instance Show FailResult where
  show r = unlines
           [ "exec failed!"
           , "<<<< process >>>>"
           , showProc (r ^. failProc)
           , "<<<< exit code >>>>"
           , show (r ^. exitcode)
           , "<<<< stdout >>>>"
           , r ^. failStdout
           , "<<<< stderr >>>>"
           , r ^. failStderr
           ]


showProc :: CreateProcess -> String
showProc p =
  case cmdspec p of
    ShellCommand s     -> s
    RawCommand fp args -> unwords [fp, unwords args]


instance Ord Version where
  compare AnyVersion  AnyVersion  = EQ
  compare AnyVersion  Latest      = LT
  compare AnyVersion  (Version _) = EQ
  compare Latest      AnyVersion  = GT
  compare Latest      Latest      = EQ
  compare Latest      (Version _) = GT
  compare (Version _) AnyVersion  = EQ
  compare (Version _) Latest      = LT
  compare (Version a) (Version b) = compareVersions a b


compareVersions :: String -> String -> Ordering
compareVersions a b = compare (ver a) (ver b)
 where
  ver x = case parseV (T.pack x) of
            Left err -> error $ "Failed to parse version '" ++ x ++ "': " ++ show err
            Right v  -> v


package :: PackageName -> Package
package n = Package n AnyVersion


latest :: PackageName -> Package
latest n = Package n Latest


