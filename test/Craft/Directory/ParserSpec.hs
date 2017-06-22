module Craft.Directory.ParserSpec where

import Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec       as MP

import Craft.Directory.Parser

spec :: Spec
spec = do
  describe "getFilesParser" $ do
    let expected = ["ab", "lkjasd", "912 12391", " ", "~"]
    it "ignores . and .." $ do
      (MP.parse getFilesParser "" $ unlines (".":"..":expected))
       `shouldParse` expected
