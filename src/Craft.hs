module Craft
( module Craft
, module Craft.Internal
, module System.FilePath
, asks
, ExitCode(..)
)
where

import Control.Monad.Reader
import System.Exit (ExitCode(..))
import System.FilePath
import Craft.Internal
import Craft.Directory as Dir
import qualified System.IO as Sys.IO


withCWD :: Dir.Directory -> Craft a -> Craft a
withCWD dir = local (\r -> r { craftExecCWD = Dir.path dir })


craftEnv :: CraftEnv NoPackageManager
craftEnv =
  CraftEnv
  { craftSourcePaths    = ["."]
  , craftPackageManager = NoPackageManager
  , craftExecEnv        = [("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")]
  , craftExecCWD        = "/"
  , craftLogHandle      = Sys.IO.stdout
  }


