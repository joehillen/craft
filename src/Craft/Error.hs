module Craft.Error where

import           Control.Monad.Catch
import           Control.Monad.Logger (logError)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Language.Haskell.TH.Syntax (Q, Exp)

data CraftError = CraftError String
  deriving Show

data CraftNotImplemented = CraftNotImplemented
  deriving Show

instance Exception CraftError

instance Exception CraftNotImplemented

-- |Log an error and throw a runtime exception
craftError :: Q Exp
craftError = [|\s -> $(logError) (T.pack s) >> throwM (CraftError s) |]


notImplemented :: Q Exp
notImplemented = [| \s -> do
  $(logError) $ "Not Implemented: " <> T.pack s
  throwM CraftNotImplemented
  |]
