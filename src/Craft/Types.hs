{-# LANGUAGE DeriveFunctor #-}
module Craft.Types where

import           Control.Monad.Reader
import           Control.Monad.Free
import           System.Exit
import           Data.ByteString (ByteString)

import           Craft.Helpers

type Craft a = forall pm. (PackageManager pm)
             => ReaderT (CraftEnv pm) (Free CraftDSL) a

data CraftEnv pm
  = CraftEnv
    { craftSourcePaths    :: [FilePath]
    , craftPackageManager :: PackageManager pm => pm
    , craftExecEnv        :: ExecEnv
    , craftExecCWD        :: FilePath
    }

type StdOut = String
type StdErr = String
type Args = [String]
type Command = FilePath

data ExecResult = ExecFail FailResult | ExecSucc SuccResult

data SuccResult = SuccResult { stdout :: StdOut
                             , stderr :: StdErr
                             }

data FailResult = FailResult { exitcode   :: Int
                             , failStdout :: StdOut
                             , failStderr :: StdErr
                             }

type ExecEnv = [(String, String)]
type CWD = FilePath

data CraftDSL next
  = Exec  CWD ExecEnv Command Args (ExecResult -> next)
  | Exec_ CWD ExecEnv Command Args next
  | FileRead FilePath (ByteString -> next)
  | FileWrite FilePath ByteString next
  | ReadSourceFile [FilePath] FilePath (ByteString -> next)
 deriving Functor

class (Eq a, Show a) => Craftable a where
  checker :: a -> Craft (Maybe a)
  crafter :: a -> Maybe a -> Craft ()
  destroyer :: a -> Craft ()

type PackageName = String

data Package =
  Package
  { pkgName    :: PackageName
  , pkgVersion :: Version
  }
  deriving (Eq, Show)


data Version
  = Version String
  | AnyVersion
  | Latest
  deriving (Show)


instance Eq Version where
  (==) AnyVersion  _           = True
  (==) _           AnyVersion  = True
  (==) Latest      (Version _) = False
  (==) (Version _) Latest      = False
  (==) (Version a) (Version b) = a == b


-- Note: This may or may not make sense.
-- Open to suggestions if any of this seems incorrect.
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
compareVersions a b = notImplemented "compareVersions"

package :: PackageName -> Package
package n = Package n AnyVersion

latest :: PackageName -> Package
latest n = Package n Latest

class PackageManager pm where
  pkgGetter      :: pm -> PackageName -> Craft (Maybe Package)
  installer      :: pm -> Package     -> Craft ()
  upgrader       :: pm -> Package     -> Craft ()
  uninstaller    :: pm -> Package     -> Craft ()
  multiInstaller :: pm -> [Package]   -> Craft ()

data NoPackageManager = NoPackageManager

instance PackageManager NoPackageManager where
  pkgGetter      _ _ = noPMerror
  installer      _ _ = noPMerror
  upgrader       _ _ = noPMerror
  uninstaller    _ _ = noPMerror
  multiInstaller _ _ = noPMerror

noPMerror :: a
noPMerror = error "NoPackageManager"

instance Craftable Package where
  checker pkg = do
    pm <- asks craftPackageManager
    pkgGetter pm $ pkgName pkg
  crafter pkg mpkg = do
    pm <- asks craftPackageManager
    case mpkg of
      Nothing -> installer pm pkg
      Just oldpkg ->
        when (pkgVersion pkg /= AnyVersion
              && (pkgVersion pkg == Latest
                  || pkgVersion oldpkg /= pkgVersion pkg)) $
          upgrader pm pkg
  destroyer pkg = do
    pm <- asks craftPackageManager
    uninstaller pm pkg
