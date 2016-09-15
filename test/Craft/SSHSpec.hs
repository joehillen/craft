module Craft.SSHSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec       as MP

import           Craft.SSH

spec :: Spec
spec = do
  describe "parseKeyType" $ do
    let parse = MP.parse parseKeyType ""
    it "fails on empty string" $
      parse `shouldFailOn` ""
    it "fails on just prefix" $
      parse `shouldFailOn` "ssh-"
    it "fails on uppercase prefix" $
      parse `shouldFailOn` "SSH-RSA"
    it "parses DSA" $
      parse "ssh-dsa" `shouldParse` DSA
    it "parses RSA" $
      parse "ssh-rsa" `shouldParse` RSA
    it "parses RSA1" $
      parse "ssh-rsa1" `shouldParse` RSA1
    it "parses ECDSA" $
      parse "ssh-ecdsa" `shouldParse` ECDSA
    it "parses ED25519" $
      parse "ssh-ed25519" `shouldParse` ED25519
    it "parses other keytypes" $
      parse "ssh-foobar" `shouldParse` (KeyType "ssh-foobar")
    it "parses mixed case" $
      parse "ssh-rSa" `shouldParse` RSA
