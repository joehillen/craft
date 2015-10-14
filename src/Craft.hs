module Craft
( module Craft
, module X
, module System.FilePath
, asks
, ExitCode(..)
)
where

import           Control.Monad.Reader
import           System.Exit (ExitCode(..))
import           System.FilePath

import           Craft.Types as X
import           Craft.Actions as X
import           Craft.Watch as X
import           Craft.Exec as X
import           Craft.Helpers as X

craftEnv :: CraftEnv NoPackageManager
craftEnv =
  CraftEnv
  { craftSourcePaths    = ["."]
  , craftPackageManager = NoPackageManager
  , craftExecEnv        = [("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")]
  , craftExecCWD        = "/"
  }

