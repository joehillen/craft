module Craft.Config.ShellSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Craft.Config.Shell


spec :: Spec
spec = do
  describe "set" $ do
    it "sets on empty" $ do
      set "a" "b" empty `shouldBe` ShellFormat [("a", "b")]
    it "replaces existing key" $ do
      set "a" "c" (ShellFormat [("a", "b")]) `shouldBe` ShellFormat [("a", "c")]
    it "appends a new keys" $ do
      set "a" "b" (ShellFormat [("b", "c")])
        `shouldBe` ShellFormat [("b", "c"), ("a", "b")]
    it "replaces multiple keys" $ do
      set "a" "c" (ShellFormat [("a", "foo"), ("a", "bar")])
      `shouldBe` ShellFormat [("a", "c"), ("a", "c")]

  describe "unset" $ do
    it "preserves empty" $ do
      unset "foo" empty `shouldBe` empty
    it "preserves unmatched keys" $ do
      unset "a" (ShellFormat [("foo", "")]) `shouldBe` ShellFormat [("foo", "")]
    it "unsets" $ do
      unset "a" (ShellFormat [("a", "b")]) `shouldBe` empty
    it "unsets multiple keys" $ do
      unset "a" (ShellFormat [("a", "b"), ("a", "c")]) `shouldBe` empty
    it "unsets multiple keys and leaves unmatched" $ do
      unset "a" (ShellFormat [("a", "b"), ("b", "foo"), ("a", "c")])
        `shouldBe` (ShellFormat [("b", "foo")])

  describe "dedup" $ do
    it "supports empty" $ do
      dedup empty `shouldBe` empty
    it "preserves single item lists" $ do
      dedup (ShellFormat [("a", "b")]) `shouldBe` ShellFormat [("a", "b")]
    it "dedup uses the final value" $ do
      dedup (ShellFormat [("a", "b"), ("a", "c")]) `shouldBe` ShellFormat [("a", "c")]
    it "dedups and preserves order" $ do
      dedup (ShellFormat [("a", "b"), ("b", ""), ("a", "c")]) `shouldBe` ShellFormat [("a", "c"), ("b", "")]

  describe "parser" $ do
    it "reads empty files" $ do
      parse parser "" "" `shouldParse` empty
    it "ignores empty lines" $ do
      parse parser "" "\n\n\n" `shouldParse` empty
    it "ignores empty lines with spaces" $ do
      parse parser "" "\n   \n\n" `shouldParse` empty
    it "ignores empty comment" $ do
      parse parser "" "#" `shouldParse` empty
    it "ignores comment" $ do
      parse parser "" "# lkajs #  akjsd\n#lkjaslkdj" `shouldParse` empty
    it "ignores comment with leading space" $ do
      parse parser "" "   # foo bar" `shouldParse` empty
    it "accepts an empty value" $ do
      parse parser "" "foo=" `shouldParse` ShellFormat [("foo", "")]
    it "accepts an value with a space in it" $ do
      parse parser "" "foo=a b " `shouldParse` ShellFormat [("foo", "a")]
    it "accepts space after equals" $ do
      parse parser "" "foo= foo" `shouldParse` ShellFormat [("foo", "")]
    it "ignores comment after a value" $ do
      parse parser "" "FOO_BAR=foobar # a comment" `shouldParse` ShellFormat [("FOO_BAR", "foobar")]
    it "ignores comment after a value with no spaces" $ do
      parse parser "" "FOO_BAR=foobar#comment" `shouldParse` ShellFormat [("FOO_BAR", "foobar")]
    it "ignores comment after empty value with no spaces" $ do
      parse parser "" "FOO_BAR=#comment" `shouldParse` ShellFormat [("FOO_BAR", "")]
    it "parses multiple values" $ do
      parse parser "" "a=a\nb=b\nc=c\n"
      `shouldParse` ShellFormat [ ("a", "a")
                                , ("b", "b")
                                , ("c", "c")
                                ]
    it "parses multiple values with an empty field" $ do
      parse parser "" "a=a\nb=\nc=c\n"
      `shouldParse` ShellFormat [ ("a", "a")
                                , ("b", "")
                                , ("c", "c")
                                ]
    it "works" $ do
      parse parser "" "#foo\na=a\nb=\n#c=c\nd= foo"
      `shouldParse` ShellFormat [ ("a", "a")
                                , ("b", "")
                                , ("d", "")
                                ]
