module Craft.Hosts.Parser where

-- |This file is just to keep Megaparsec from conflicting with other modules

import           Control.Monad                  (void)
import           Data.List                      (intercalate)
import           Data.Maybe                     (catMaybes)
import           Data.Void                      (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Craft.Hosts.Types
import           Craft.Internal.Helpers.Parsing (end)
import           Craft.Types


parseHosts :: String -> Craft Hosts
parseHosts s = do
  case runParser parser (fromAbsFile hostsfp) s of
    Right x  -> return x
    Left err -> $craftError $ show err


parser :: Parsec Void String Hosts
parser = do
  rs <- parserLine `sepEndBy` (many eol)
  return . Hosts $ catMaybes rs


parserLine :: Parsec Void String (Maybe (IP, [Name]))
parserLine = do
  -- skip comments
  try (comment >> return Nothing)
  -- skip blank lines even if they contain spaces
  <|> try (skipMany white >> lookAhead eol >> return Nothing)
  <|> (Just <$> item)


comment :: Parsec Void String String
comment = do
  skipMany white
  void $ char '#'
  manyTill anyChar end


item :: Parsec Void String (IP, [Name])
item = do
  skipMany white
  ip <- try ipv4 <|> ipv6
  skipSome white
  names <- hostname `sepEndBy1` many white
  void $ optional $ try comment
  return (ip, names)


num :: Parsec Void String String
num = choice [ string "0", nonzero ]


nonzero :: Parsec Void String String
nonzero = do
  a <- oneOf ['1'..'9'] <?> "non-zero digit with non-zero part"
  rest <- count' 0 2 digitChar
  if (read (a:rest) :: Int) > 255
    then fail "IP address parts must be 0 <= x <= 255"
    else return (a:rest)


-- TODO: check RFC
hostname :: Parsec Void String Name
hostname = Name <$> some (alphaNumChar <|> oneOf ['.', '-'])


dot :: Parsec Void String ()
dot = void $ char '.'


ipv4 :: Parsec Void String IP
ipv4 = label "ipv4 address" $ do
  p1 <- num
  dot
  p2 <- num
  dot
  p3 <- num
  dot
  p4 <- num
  notFollowedBy digitChar
  return . IP $ intercalate "." [p1, p2, p3, p4]


ipv6 :: Parsec Void String IP
ipv6 = IP <$> some (hexDigitChar <|> char ':') <?> "ipv6 address"


white :: Parsec Void String Char
white = oneOf [' ', '\t']
