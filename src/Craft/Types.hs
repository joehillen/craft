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
import           Control.Monad              (unless)
import           Control.Monad.Catch        (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Logger       (LoggingT, MonadLogger, logDebugNS,
                                             monadLoggerLog)
import           Control.Monad.Reader       (MonadReader, ReaderT, runReaderT)
import qualified Control.Monad.Trans.Class  as Trans
import           Control.Monad.Trans.Free   (FreeT, MonadFree, iterT)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as B8
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (isNothing)
import qualified Data.Text                  as T
import qualified Data.Versions
import           Formatting
import           Language.Haskell.TH.Syntax (Exp, Q)
import           Path                       hiding (File)
import qualified Path
import           Prelude                    hiding (FilePath)
import qualified Prelude
import           System.Directory           (doesFileExist)
import           System.Process

import           Craft.Error
import           Craft.File.Mode
import           Craft.Internal.Helpers


-- | Shortened aliases for Path types for convenience
type AbsFilePath = Path Abs Path.File
type RelFilePath = Path Rel Path.File
type AbsDirPath  = Path Abs Dir
type RelDirPath  = Path Rel Dir


data CraftEnv
  = CraftEnv
  { _craftPackageManager :: PackageManager
  , _craftExecEnvVars    :: ExecEnvVars
  , _craftCWD            :: AbsDirPath
  , _craftUserID         :: UserID
  }
  deriving Show


craftEnv :: PackageManager -> CraftEnv
craftEnv pm =
  CraftEnv
  { _craftPackageManager = pm
  , _craftExecEnvVars    = Map.fromList [("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")]
  , _craftCWD            = $(mkAbsDir "/")
  , _craftUserID         = rootUserID
  }


newtype Craft a = Craft { unCraft :: ReaderT CraftEnv (FreeT CraftDSL (LoggingT IO)) a }
  deriving ( Functor, Monad, MonadIO, Applicative, MonadReader CraftEnv
           , MonadFree CraftDSL, MonadThrow, MonadCatch, MonadLogger)


instance (MonadLogger m, Functor f) => MonadLogger (FreeT f m) where
  monadLoggerLog a b c d = Trans.lift $ monadLoggerLog a b c d


data CraftRunner = CraftRunner
  { runExec       :: CraftEnv -> Command -> Args -> LoggingT IO ExecResult
  , runExec_      :: CraftEnv -> Command -> Args -> LoggingT IO ()
  , runFileRead   :: AbsFilePath -> LoggingT IO ByteString
  , runFileWrite  :: AbsFilePath -> ByteString -> LoggingT IO ()
  , runSourceFile :: Prelude.FilePath -> AbsFilePath -> LoggingT IO ()
  }


runCraft :: CraftRunner -> CraftEnv -> Craft a -> LoggingT IO a
runCraft runner ce dsl =
  iterT (interpreter runner) $ flip runReaderT ce $ unCraft dsl

interpreter :: CraftRunner -> CraftDSL (LoggingT IO a) -> LoggingT IO a
interpreter runner (Exec ce cmd args next) = do
  logDebugNS "Exec" . T.unwords . map T.pack $ (show ce):cmd:args
  (runExec runner) ce cmd args >>= next
interpreter runner (Exec_ ce cmd args next) = do
  logDebugNS "Exec_" . T.unwords . map T.pack $ (show ce):cmd:args
  (runExec_ runner) ce cmd args >> next
interpreter runner (FileRead fp next) = do
  logDebugNS "FileRead" $ sformat shown fp
  (runFileRead runner) fp >>= next
interpreter runner (FileWrite fp content next) = do
  logDebugNS "FileWrite" $ sformat string $ fromAbsFile fp
  (runFileWrite runner) fp content >> next
interpreter runner (SourceFile sourcer dest next) = do
  src <- Trans.lift sourcer
  logDebugNS "SourceFile" $ sformat (string%" "%string) src $ fromAbsFile dest
  !exists <- Trans.lift $ doesFileExist src
  unless exists $ do
    error $ "Failed to source file! " ++ src ++ " does not exist!"
  (runSourceFile runner) src dest >> next


type StdOut  = String
type StdErr  = String
type Args    = [String]
type Command = String


data SuccResult
  = SuccResult
  { _stdout      :: StdOut
  , _stderr      :: StdErr
  , _succProcess :: CreateProcess
  }


data FailResult
  = FailResult
  { _exitcode    :: Int
  , _failStdout  :: StdOut
  , _failStderr  :: StdErr
  , _failProcess :: CreateProcess
  }


data ExecResult
  = Failure FailResult
  | Success SuccResult


isSuccess :: ExecResult -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False


isFailure :: ExecResult -> Bool
isFailure = not . isSuccess


errorOnFail :: Q Exp
errorOnFail = [|
  \case
    Success r -> return r
    Failure r -> $craftError $ show r|]


-- | Try to get STDOUT from a process.
-- If the command exits with an error code, throw a CraftError.
stdoutOrError :: Q Exp
stdoutOrError = [|
  \case
    Success r -> return $ _stdout r
    Failure r -> $craftError $ show r|]


type ExecEnvVars = Map String String
type CWD = AbsFilePath
type PackageName = String

newtype UserName = UserName { _userNameString :: String }
                   deriving (Eq, Ord)

instance Show UserName where
  show (UserName n) = n

newtype UserID = UserID { _userIDInt :: Int }
                 deriving (Eq, Ord)

instance Show UserID where
  show (UserID n) = show n

rootUserID :: UserID
rootUserID = UserID 0

instance ToArg UserID

newtype GroupName = GroupName { _groupNameString :: String }
                    deriving (Eq, Ord)

instance Show GroupName where
  show (GroupName n) = n

newtype GroupID = GroupID { _groupIDInt :: Int }
                  deriving (Eq, Ord)

instance Show GroupID where
  show (GroupID n) = show n

rootGroupID :: GroupID
rootGroupID = GroupID 0

instance ToArg GroupID


class Eq (FileLikePath a) => FileLike a where
  type FileLikePath a :: *
  path    :: Lens' a (FileLikePath a)
  mode    :: Lens' a Mode
  ownerID :: Lens' a UserID
  groupID :: Lens' a GroupID


data File
  = File
  { _filePath    :: AbsFilePath
  , _fileMode    :: Mode
  , _fileOwnerID :: UserID
  , _fileGroupID :: GroupID
  , _fileContent :: Maybe ByteString
  }


file :: AbsFilePath -> File
file fp =
  File
  { _filePath    = fp
  , _fileMode    = Mode RW R R
  , _fileOwnerID = rootUserID
  , _fileGroupID = rootGroupID
  , _fileContent = Nothing
  }



data Directory
  = Directory
  { _directoryPath    :: AbsDirPath
  , _directoryMode    :: Mode
  , _directoryOwnerID :: UserID
  , _directoryGroupID :: GroupID
  }
  deriving (Show, Eq)


directory :: AbsDirPath -> Directory
directory dp =
  Directory
  { _directoryPath    = dp
  , _directoryMode    = Mode RWX RX RX
  , _directoryOwnerID = rootUserID
  , _directoryGroupID = rootGroupID
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
   { _pmName        :: String
   , _pmGetter      :: PackageName -> Craft (Maybe Package)
   , _pmInstaller   :: Package     -> Craft ()
   , _pmUpgrader    :: Package     -> Craft ()
   , _pmUninstaller :: Package     -> Craft ()
   }

instance Show PackageManager where
  show pm = _pmName pm


noPackageManager :: PackageManager
noPackageManager =
  let name = "No Package Manager"
      err :: forall a b. a -> Craft b
      err _ = $craftError name
  in
  PackageManager
  { _pmName           = name
  , _pmGetter         = err
  , _pmInstaller      = err
  , _pmUpgrader       = err
  , _pmUninstaller    = err
  }


data CraftDSL next
  = Exec CraftEnv Command Args (ExecResult -> next)
  | Exec_ CraftEnv Command Args next
  | FileRead AbsFilePath (ByteString -> next)
  | FileWrite AbsFilePath ByteString next
  | SourceFile (IO Prelude.FilePath) AbsFilePath next
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
makeLenses ''UserName
makeLenses ''UserID
makeLenses ''GroupName
makeLenses ''GroupID


fileName :: Lens' File RelFilePath
fileName =
  lens
    (view $ path.to filename)
    (\s fn -> s & path .~ ((s^.path.to parent)</>fn))

strContent :: Setter File File () String
strContent = sets (\functor f -> f & fileContent .~ Just (B8.pack (functor ())))


instance Eq File where
  (==) a b =
    (a ^. filePath == b ^. filePath)
    && (a ^. fileMode == b ^. fileMode)
    && (a ^. fileOwnerID == b ^. fileOwnerID)
    && (a ^. fileGroupID == b ^. fileGroupID)
    && (  isNothing (a ^. fileContent)
      || isNothing (b ^. fileContent)
      || (a ^. fileContent == b ^. fileContent))


instance Show File where
  show f =
    "File { _filePath = " ++ show (f ^. filePath) ++
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
  type FileLikePath File = AbsFilePath
  path = filePath
  mode = fileMode
  ownerID = fileOwnerID
  groupID = fileGroupID


instance FileLike Directory where
  type FileLikePath Directory = AbsDirPath
  path = directoryPath
  mode = directoryMode
  ownerID = directoryOwnerID
  groupID = directoryGroupID


dirName :: Lens' Directory RelDirPath
dirName =
  lens
    (view $ path.to dirname)
    (\s fn -> s & path .~ ((s^.path.to parent)</>fn))


data User
  = User
    { _userName         :: UserName
    , _uid              :: UserID
    , _userComment      :: String
    , _userGroup        :: Group
    , _userGroups       :: [GroupName]
    , _userHome         :: AbsDirPath
    , _userPasswordHash :: String
    --, _salt         :: String
    --, _locked       :: Bool
    , _userShell        :: AbsFilePath
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
owner = sets (\functor filelike -> filelike & ownerID .~ ((functor ()) ^. uid))


group :: FileLike a => Setter a a () Group
group =
  sets (\functor filelike -> doit filelike (functor ()))
 where
   doit filelike g = filelike & groupID .~ (g ^. gid)


ownerAndGroup :: FileLike a => Setter a a () User
ownerAndGroup =
  sets (\functor filelike -> doit filelike (functor ()))
 where
  doit filelike u =
    filelike
    & owner .~ u
    & group .~ (u ^. userGroup)


execResultProcess :: ExecResult -> CreateProcess
execResultProcess (Failure r) = r^.failProcess
execResultProcess (Success r) = r^.succProcess


instance Show FailResult where
  show r =
    unlines
    [ "exec failed!"
    , "<<<< process >>>>"
    , showProcess $ r^.failProcess
    , "<<<< exit code >>>>"
    , show $ r^.exitcode
    , "<<<< stdout >>>>"
    , r^.failStdout
    , "<<<< stderr >>>>"
    , r^.failStderr
    ]


showProcess :: CreateProcess -> String
showProcess p =
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
compareVersions a b =
  compare (ver a) (ver b)
 where
  ver x =
    case Data.Versions.version (T.pack x) of
      Left err -> error $ "Failed to parse version '" ++ x ++ "': " ++ show err
      Right v  -> v


package :: PackageName -> Package
package n = Package n AnyVersion


latest :: PackageName -> Package
latest n = Package n Latest


