module Craft.SSH.AuthorizedKeySpec (spec) where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec as MP

import           Craft.SSH
import           Craft.SSH.PublicKey
import           Craft.SSH.AuthorizedKey


spec :: Spec
spec = do
  describe "parsePublicKeys" $ do
    let parse = MP.parse parsePublicKeys ""
    it "reads empty file" $
      parse "" `shouldParse` []
    it "reads empty lines" $
      parse "\n\n\n" `shouldParse` []
    it "reads single RSA public key with no eol or comment" $
      parse "ssh-rsa AAAiumas98dada8ud9m8ad" `shouldParse` [PublicKey RSA "AAAiumas98dada8ud9m8ad" ""]
    it "reads single DSA public key ending in newline" $
      parse "ssh-dsa AAAiumas98dada8ud9m8ad\n" `shouldParse` [PublicKey DSA "AAAiumas98dada8ud9m8ad" ""]
    it "reads single public key with comment ending in newline" $
      parse "ssh-ed25519 AAAiumas98dada8ud9m8ad ED25519\n" `shouldParse` [PublicKey ED25519 "AAAiumas98dada8ud9m8ad" "ED25519"]
    it "reads multiple keys" $ do
      parse "ssh-rsa AAAiumas98dada8ud9m8ad a comment\nssh-dsa M123YMOSDlkjsadiuasoim8asdoum"
        `shouldParse` [ PublicKey RSA "AAAiumas98dada8ud9m8ad" "a comment"
                      , PublicKey DSA "M123YMOSDlkjsadiuasoim8asdoum" ""
                      ]
