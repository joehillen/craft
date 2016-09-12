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

-- |Re-exports
import           Control.Monad.Catch  as X
import           System.Exit          (ExitCode (..))
import           System.FilePath      as X

import           Craft.Craftable      as X
import           Craft.Internal       as X


withCWD :: Directory -> Craft a -> Craft a
withCWD dir = local (\r -> r & craftExecCWD .~ dir ^. directoryPath)


