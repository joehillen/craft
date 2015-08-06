module Craft.Helpers where

import qualified Data.String.QQ as SQQ
import qualified System.IO as IO
import           System.Console.ANSI
import           Language.Haskell.TH.Quote (QuasiQuoter, quoteFile)


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

multiline :: QuasiQuoter
multiline = SQQ.s

raw_f :: QuasiQuoter
raw_f = quoteFile SQQ.s

notImplemented :: String -> a
notImplemented m = error $ "ERROR: Not Implemented! " ++ m

trimTrailing :: String -> String
trimTrailing = reverse . dropWhile (`elem` ("\n\r\t " :: String)) . reverse
