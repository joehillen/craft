module Craft.File.ModeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Craft.File.Mode


spec :: Spec
spec = do
  describe "toFileMode" $
    prop "is inverse of toMode" $
      \x ->  (x :: Mode) == (toMode . toFileMode) x
  describe "toOctalString" $
    prop "is inverse of fromOctalString" $
      \x ->  (x :: Mode) == (fromOctalString . toOctalString) x
