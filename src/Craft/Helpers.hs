module Craft.Helpers
( module Craft.Helpers
, when
, unless
, void
, md5
)
where

import Control.Monad (when, unless, void)
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


whenM :: Monad m => m Bool -> m () -> m ()
whenM mbool action = mbool >>= flip when action


unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action


whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  _   = return ()
whenJust (Just v) act = act v
