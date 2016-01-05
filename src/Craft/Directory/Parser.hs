module Craft.Directory.Parser where

-- |This file is just to keep Megaparsec from conflicting with other modules

import Control.Monad (void)
import Data.List (intercalate)
import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text as T

getFilesParser :: Parsec Text [FilePath]
getFilesParser = stuff `sepBy` newline <* optional newline
 where
  stuff :: Parsec Text FilePath
  stuff = do
    void $ optional $ string "." >> newline
    void $ optional $ string ".." >> newline
    line
  line :: Parsec Text String
  line = many $ noneOf "\n"


testGetFilesParser :: IO Bool
testGetFilesParser = do
  let expected = ["ab", "lkjasd", "912 12391", " ", "~"] :: [FilePath]
  let resultE = parse getFilesParser "testGetFilesParser"
                $ T.pack $ intercalate "\n" (".":"..":expected)
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
