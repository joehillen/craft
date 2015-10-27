module Craft.Helpers where

import qualified System.IO as IO
import           System.Console.ANSI


color :: Color -> IO a -> IO a
color c action = do
  setSGR [SetColor Foreground Vivid c]
  r <- action
  setSGR [Reset]
  return r

msg :: String -> String -> IO ()
msg name m = do
  color Green $ putStr $ ">>= " ++ name ++ " "
  putStrLn m
  IO.hFlush IO.stdout


notImplemented :: String -> a
notImplemented m = error $ "ERROR: Not Implemented! " ++ m


trimTrailing :: String -> String
trimTrailing = reverse . dropWhile (`elem` ("\n\r\t " :: String)) . reverse


appendNL :: String -> String
appendNL s =
  s ++ if not (null s) then "\n" else ""
