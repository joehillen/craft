module Craft.Run.SSHSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((==>))

import Craft.Run.SSH (escape, specialChars)

spec :: Spec
spec = do
  describe "escape" $ do
    prop "does not escape normal character" $
      \s ->  all (`notElem` specialChars) s ==> s == escape s
    it "escapes backslashes" $
      escape "\\" `shouldBe` "\\\\"
    it "escapes whitespace" $
      escape "foo bar" `shouldBe` "foo\\ bar"
    it "escapes" $
      escape "--showformat=\"${Status}\"" `shouldBe` "--showformat=\\\"\\$\\{Status\\}\\\""
