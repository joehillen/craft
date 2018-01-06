module Craft.Directory.Parser where

-- |This file is just to keep Megaparsec from conflicting with other modules

import           Control.Monad          (void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Void (Void)


getFilesParser :: Parsec Void String [String]
getFilesParser = do
  void $ optional $ char '.' >> newline
  void $ optional $ string ".." >> newline
  r <- (some $ noneOf ['\n']) `sepEndBy` newline
  space
  return r
