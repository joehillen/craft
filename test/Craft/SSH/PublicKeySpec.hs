module Craft.SSH.PublicKeySpec (spec) where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec as MP

import           Craft.SSH
import           Craft.SSH.PublicKey


spec :: Spec
spec = do
  describe "parsePublicKey" $ do
    let parse = MP.parse parsePublicKey ""
    it "fails to read empty string" $
      parse `shouldFailOn` ""
    it "fails to read just keytype" $
      parse `shouldFailOn` "ssh-rsa "
    it "reads RSA public key with no eol or comment" $
      parse "ssh-rsa AAAiumas98dada8ud9m8ad" `shouldParse` (PublicKey RSA "AAAiumas98dada8ud9m8ad" "")
    it "reads RSA public key with comment and no eol" $
      parse "ssh-rsa AAA/umas98dad+8ud9m8ad foo@bar" `shouldParse` (PublicKey RSA "AAA/umas98dad+8ud9m8ad" "foo@bar")
    it "reads DSA public key ending in newline" $
      parse "ssh-dsa AAAiumas98dad/8ud9m8ad=\n" `shouldParse` (PublicKey DSA "AAAiumas98dad/8ud9m8ad=" "")
    it "reads public key with comment ending in newline" $
      parse "ssh-ed25519 AAAiumas9+dada8ud9m8ad== ED25519\n" `shouldParse` (PublicKey ED25519 "AAAiumas9+dada8ud9m8ad==" "ED25519")
