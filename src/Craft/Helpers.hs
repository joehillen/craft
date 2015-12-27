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

import Control.Monad (when, unless, void)
import Control.Monad.Extra (whenM, unlessM)
import qualified System.IO as IO
import System.Console.ANSI
import Data.Digest.Pure.MD5 (md5)


color :: Color -> IO a -> IO a
color c action = do
  setSGR [SetColor Foreground Vivid c]
  r <- action
  setSGR [Reset]
  return r


trimTrailing :: String -> String
trimTrailing = reverse . dropWhile (`elem` ("\n\r\t " :: String)) . reverse


appendNL :: String -> String
appendNL s =
  s ++ if not (null s) then "\n" else ""


whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  _   = return ()
whenJust (Just v) act = act v
