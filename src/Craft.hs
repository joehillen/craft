module Craft
( module Craft
, module Craft.Internal
, module System.FilePath
, asks
, ExitCode(..)
)
where

import Control.Lens
import Control.Monad.Reader
import System.Exit (ExitCode(..))
import System.FilePath
import Craft.Internal
import Craft.Directory as Dir
import qualified System.IO as Sys.IO


withCWD :: Dir.Directory -> Craft a -> Craft a
withCWD dir = local (\r -> r & craftExecCWD .~ dir ^. Dir.path)


craftEnv :: CraftEnv NoPackageManager
craftEnv =
  CraftEnv
  { _craftSourcePaths    = ["."]
  , _craftPackageManager = NoPackageManager
  , _craftExecEnv        = [("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")]
  , _craftExecCWD        = "/"
  , _craftLogger         = craftDefaultLogger Sys.IO.stdout LevelDebug
  }
