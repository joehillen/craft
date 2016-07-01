module Craft
( module Craft
, asks
, ExitCode(..)
, module X
-- Control.Lens
, (&)
, (.~)
, (^.)
, (%~)
, (?~)
, view
, to
)
where

import           Control.Lens
import           Control.Monad.Reader
import qualified Data.Map.Strict       as Map

-- |Re-exports
import           Control.Monad.Catch   as X
import           System.FilePath       as X
import           System.Exit           (ExitCode(..))

import           Craft.Internal        as X
import           Craft.Craftable       as X


withCWD :: Directory -> Craft a -> Craft a
withCWD dir = local (\r -> r & craftExecCWD .~ dir ^. directoryPath)


craftEnv :: PackageManager -> CraftEnv
craftEnv pm =
  CraftEnv
  { _craftSourcePaths    = ["."]
  , _craftPackageManager = pm
  , _craftExecEnv        = Map.fromList [("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")]
  , _craftExecCWD        = "/"
  }
