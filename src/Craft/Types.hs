{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Craft.Types
( module Craft.Types
, module Craft.Error
)
where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger (LoggingT, MonadLogger, monadLoggerLog)
import           Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import qualified Control.Monad.Trans.Class as Trans
import           Control.Monad.Trans.Free (FreeT, MonadFree, iterT)
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Versions (parseV)
import           Language.Haskell.TH.Syntax (Q, Exp)
import           System.Process

import           Craft.Error
import           Craft.Helpers


newtype Craft a = Craft { unCraft :: ReaderT CraftEnv (FreeT CraftDSL (LoggingT IO)) a }
  deriving ( Functor, Monad, MonadIO, Applicative, MonadReader CraftEnv
           , MonadFree CraftDSL, MonadThrow, MonadCatch, MonadLogger)


instance (MonadLogger m, Functor f) => MonadLogger (FreeT f m) where
  monadLoggerLog a b c d = Trans.lift $ monadLoggerLog a b c d


interpretCraft :: CraftEnv -> (CraftDSL (LoggingT IO a) -> LoggingT IO a) -> Craft a -> LoggingT IO a
interpretCraft ce interpreter = iterT interpreter . flip runReaderT ce . unCraft


type StdOut  = String
type StdErr  = String
type Args    = [String]
type Command = FilePath


data Watched
  = Unchanged
  | Created
  | Updated
  | Removed
  deriving (Eq, Show)


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


type ExecEnv = [(String, String)]
type CWD = FilePath
type PackageName = String


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
   { _pmGetter         :: PackageName -> Craft (Maybe Package)
   , _pmInstaller      :: Package     -> Craft ()
   , _pmUpgrader       :: Package     -> Craft ()
   , _pmUninstaller    :: Package     -> Craft ()
   }


noPackageManager :: PackageManager
noPackageManager = let err _ = $craftError "No Package Manager" in
  PackageManager
  { _pmGetter         = err
  , _pmInstaller      = err
  , _pmUpgrader       = err
  , _pmUninstaller    = err
  }


data CraftEnv
  = CraftEnv
    { _craftPackageManager :: PackageManager
    , _craftSourcePaths    :: [FilePath]
    , _craftExecEnv        :: ExecEnv
    , _craftExecCWD        :: FilePath
    }


data CraftDSL next
  = Exec  CraftEnv Command Args (ExecResult -> next)
  | Exec_ CraftEnv Command Args next
  | FileRead CraftEnv FilePath (ByteString -> next)
  | FileWrite CraftEnv FilePath ByteString next
  | SourceFile CraftEnv FilePath FilePath next
  | FindSourceFile CraftEnv FilePath ([FilePath] -> next)
  | ReadSourceFile CraftEnv FilePath (ByteString -> next)
 deriving Functor


makeLenses ''PackageManager
makeLenses ''CraftEnv
makePrisms ''Watched
makeLenses ''Package
makePrisms ''Version
makeLenses ''FailResult
makeLenses ''SuccResult


class Craftable a where
  watchCraft :: a -> Craft (Watched, a)

  craft :: a -> Craft a
  craft x = snd <$> watchCraft x

  craft_ :: a -> Craft ()
  craft_ = void . craft

  watchCraft_ :: a -> Craft Watched
  watchCraft_ x = fst <$> watchCraft x

  {-# MINIMAL watchCraft #-}


class Destroyable a where
  watchDestroy :: a -> Craft (Watched, Maybe a)

  destroy :: a -> Craft (Maybe a)
  destroy x = snd <$> watchDestroy x

  destroy_ :: a -> Craft ()
  destroy_ = void . destroy

  watchDestroy_ :: a -> Craft Watched
  watchDestroy_ x = fst <$> watchDestroy x

  {-# MINIMAL watchDestroy #-}


execResultProc :: ExecResult -> CreateProcess
execResultProc (ExecFail failr) = failr ^. failProc
execResultProc (ExecSucc succr) = succr ^. succProc


instance Show FailResult where
  show r = concatMap appendNL [ "exec failed!"
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
    ShellCommand s -> s
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


instance Craftable Package where
  watchCraft pkg = do
    ce <- ask
    let pm       = ce ^. craftPackageManager
    let name     = pkg ^. pkgName
    let version  = pkg ^. pkgVersion
    let get      = (pm ^. pmGetter) name
    let install  = (pm ^. pmInstaller) pkg
    let upgrade  = (pm ^. pmUpgrader) pkg
    let pkgError = "craft Package `" ++ name ++ "` failed! "
    let notFound = pkgError ++ "Not Found."
    let wrongVersion got = pkgError ++ "Wrong Version: " ++ show got ++ " Excepted: " ++ show version
    get >>= \case                                                                -- Is the package installed?
      Nothing           -> do                                                    -- It's not installed.
        install                                                                  -- Install it.
        get >>= \case                                                            -- Verify the installation.
          Nothing           -> $craftError notFound                              -- Not Found. The install failed!
          Just installedPkg ->
            let ok = return (Created, installedPkg)
            in case version of                                                   -- Make sure it's the right version
                 AnyVersion -> ok
                 Latest     -> ok
                 Version  _ -> if version == installedPkg ^. pkgVersion
                                then ok
                                else $craftError $ wrongVersion (installedPkg ^. pkgVersion)
      Just installedPkg -> do                                                    -- Package was already installed.
        let installedVersion = installedPkg ^. pkgVersion
        case version of
          AnyVersion -> return (Unchanged, installedPkg)
          Latest     -> do                                                       -- Ensure it's the latest version.
            upgrade
            get >>= \case
              Nothing          -> $craftError notFound                           -- Not found. Where did it go?
              Just upgradedPkg ->
                return $ if upgradedPkg ^. pkgVersion > installedPkg ^. pkgVersion -- If the package version increased,
                          then (Updated, upgradedPkg)                            -- Then the package was upgraded
                          else (Unchanged, upgradedPkg)                          -- Else it was already the latest.
          Version _  ->                                                          -- Expecting a specific version
            if version == installedVersion                                       -- Is the correct version installed?
             then return (Unchanged, installedPkg)
             else do
               upgrade                                                           -- Try upgrading to the correct version.
               get >>= \case
                 Nothing          -> $craftError notFound                        -- Where did it go?
                 Just upgradedPkg -> if version == upgradedPkg ^. pkgVersion     -- Is the correct version installed?
                                      then return (Updated, upgradedPkg)
                                      else $craftError $ wrongVersion (upgradedPkg ^. pkgVersion)


instance Destroyable Package where
  watchDestroy pkg = do
    ce <- ask
    let pm   = ce ^. craftPackageManager
    let name = pkg ^. pkgName
    let get  = (pm ^. pmGetter) name
    get >>= \case
      Nothing -> return (Unchanged, Nothing)
      Just installedPkg -> do
        (pm ^. pmUninstaller) pkg
        get >>= \case
          Nothing            -> return (Removed, Just installedPkg)
          Just unexpectedPkg -> $craftError $ "destroy Package `" ++ name ++ "` failed! " ++ "Found: " ++ show unexpectedPkg


changed :: Watched -> Bool
changed = not . unchanged


unchanged :: Watched -> Bool
unchanged Unchanged = True
unchanged _         = False


created :: Watched -> Bool
created Created = True
created _       = False


updated :: Watched -> Bool
updated Updated = True
updated _       = False


removed :: Watched -> Bool
removed Removed = True
removed _       = False
