module Craft
( module Craft
, module Craft.Internal
, module System.FilePath
, asks
, catch
, throwM
, ExitCode(..)
)
where

import           Control.Lens
import           Control.Monad.Catch  (catch, throwM)
import           Control.Monad.Reader
import           Craft.Directory      as Dir
import           Craft.Internal
import           System.Exit          (ExitCode (..))
import           System.FilePath


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
