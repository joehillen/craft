module Craft.SSH.PublicKey where

import           Control.Lens
import           Data.Void                      (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Craft
import           Craft.Internal.Helpers.Parsing
import           Craft.SSH


data PublicKey
  = PublicKey
    { _pubkeyType    :: KeyType
    , _pubkeyValue   :: String
    , _pubkeyComment :: String
    }
    deriving (Show, Eq)
makeLenses ''PublicKey


publicKey :: KeyType -> String -> PublicKey
publicKey pktype pkval =
  PublicKey
  { _pubkeyType    = pktype
  , _pubkeyValue   = pkval
  , _pubkeyComment = ""
  }


toString :: PublicKey -> String
toString pk =
  let comment = pk ^. pubkeyComment
  in show (pk ^. pubkeyType) ++ " " ++ pk ^. pubkeyValue
     ++ if null comment then "" else " " ++ comment


parsePublicKey :: Parsec Void String PublicKey
parsePublicKey = do
  space
  kt <- parseKeyType
  space
  key <- base64Char `someTill` (void spaceChar <|> eof)
  space
  comment <- anyChar `manyTill` end
  return $ PublicKey kt key comment


base64Char :: Parsec Void String Char
base64Char = alphaNumChar <|> char '+' <|> char '/' <|> char '='
