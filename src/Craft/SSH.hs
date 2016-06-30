module Craft.SSH where

import           Control.Lens
import           Data.Char (toLower)
import           Text.Megaparsec
import           Text.Megaparsec.String

import           Craft


data KeyType
  = DSA
  | RSA
  | RSA1
  | ECDSA
  | ED25519
  | KeyType String
  deriving (Eq)


instance Show KeyType where
  show DSA         = "ssh-dsa"
  show RSA         = "ssh-rsa"
  show RSA1        = "ssh-rsa1"
  show ECDSA       = "ssh-ecdsa"
  show ED25519     = "ssh-ed25519"
  show (KeyType s) = s


userDir :: User -> Directory
userDir user =
  directory ((user^.userHome)</>".ssh")
  & mode          .~ Mode RWX O O
  & ownerAndGroup .~ user


parseKeyType :: Parser KeyType
parseKeyType = do
  void $ string "ssh-"
  s <- alphaNumChar `someTill` (void spaceChar <|> eof)
  -- The reason for converting the string to lowercase is because
  -- KeyType is allowed to be any string.
  -- This prevents "ssh-rSa" from parsing to `KeyType "ssh-rSa"` instead of `RSA`
  -- even if "ssh-rSa" is not technically valid.
  return $ case map toLower s of
    "dsa"     -> DSA
    "rsa"     -> RSA
    "rsa1"    -> RSA1
    "ecdsa"   -> ECDSA
    "ed25519" -> ED25519
    x         -> KeyType ("ssh-"++x)
