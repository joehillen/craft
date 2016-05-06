module Craft
( module Craft
, module Craft.Internal
, module System.FilePath
, module Control.Monad.Catch
, asks
, ExitCode(..)
)
where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           System.Exit          (ExitCode (..))
import           System.FilePath

import           Craft.Directory      as Dir
import           Craft.Internal


withCWD :: Dir.Directory -> Craft a -> Craft a
withCWD dir = local (\r -> r & craftExecCWD .~ dir ^. Dir.path)


craftEnv :: PackageManager -> CraftEnv
craftEnv pm =
  CraftEnv
  { _craftSourcePaths    = ["."]
  , _craftPackageManager = pm
  , _craftExecEnv        = [("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")]
  , _craftExecCWD        = "/"
  }
