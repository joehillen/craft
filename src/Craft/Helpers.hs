module Craft.Helpers
( module Craft.Helpers
, when
, unless
, void
, whenM
, unlessM
, md5
)
where

import Data.Monoid ((<>))
import Control.Monad (when, unless, void)
import Control.Monad.Extra (whenM, unlessM)
import System.Console.ANSI
import Data.Digest.Pure.MD5 (md5)
import Data.Text as T


color :: Color -> IO a -> IO a
color c action = do
  setSGR [SetColor Foreground Vivid c]
  r <- action
  setSGR [Reset]
  return r


trimTrailing :: Text -> Text
trimTrailing = dropWhileEnd (`elem` ("\n\r\t " :: String))


whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  _   = return ()
whenJust (Just v) act = act v
