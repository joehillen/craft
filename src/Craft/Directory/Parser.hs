module Craft.Directory.Parser where

-- |This file is just to keep Megaparsec from conflicting with other modules

import           Control.Monad   (void)
import           Text.Megaparsec


getFilesParser :: Parsec String [String]
getFilesParser = stuff `sepBy` newline <* optional newline
 where
  stuff :: Parsec String String
  stuff = do
    void $ optional $ string "." >> newline
    void $ optional $ string ".." >> newline
    line
  line :: Parsec String String
  line = many $ noneOf "\n"


testGetFilesParser :: IO Bool
testGetFilesParser = do
  let expected = ["ab", "lkjasd", "912 12391", " ", "~"] :: [String]
  let resultE = parse getFilesParser "testGetFilesParser" $ unlines (".":"..":expected)
  case resultE of
    Left err -> do
      putStrLn $ "FAILED: error " ++ show err
      return False
    Right result ->
      if result /= expected then do
        putStrLn $ "FAILED: got " ++ show expected
        return False
      else
        return True
