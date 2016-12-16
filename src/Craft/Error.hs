module Craft.Error where

import           Control.Monad.Catch
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Language.Haskell.TH.Syntax (Exp, Q)
import           Log                        (logAttention_)

data CraftError = CraftError String
  deriving Show

data CraftNotImplemented = CraftNotImplemented
  deriving Show

instance Exception CraftError

instance Exception CraftNotImplemented

-- |Log an error and throw a runtime exception
craftError :: Q Exp
craftError = [|\s -> logAttention_ (T.pack s) >> throwM (CraftError s) |]


notImplemented :: Q Exp
notImplemented = [| \s -> do
  logAttention_ $ "Not Implemented: " <> T.pack s
  throwM CraftNotImplemented
  |]
