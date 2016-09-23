module Craft.Hosts.Parser where

-- |This file is just to keep Megaparsec from conflicting with other modules

import           Control.Monad                  (void)
import           Data.List                      (intercalate)
import           Data.Maybe                     (catMaybes)
import           Text.Megaparsec
-- import           Text.Megaparsec.Lexer
import           Text.Megaparsec.String

import           Craft.Hosts.Types
import           Craft.Internal.Helpers.Parsing (end)
import           Craft.Types


parseHosts :: String -> Craft Hosts
parseHosts s = do
  case runParser parser hostsfp s of
    Right x  -> return x
    Left err -> $craftError $ show err


parser :: Parser Hosts
parser = do
  rs <- parserLine `sepEndBy` (many eol)
  return . Hosts $ catMaybes rs


parserLine :: Parser (Maybe (IP, [Name]))
parserLine = do
  -- skip comments
  try (comment >> return Nothing)
  -- skip blank lines even if they contain spaces
  <|> try (skipMany white >> lookAhead eol >> return Nothing)
  <|> (Just <$> item)


comment :: Parser String
comment = do
  skipMany white
  void $ char '#'
  manyTill anyChar end


item :: Parser (IP, [Name])
item = do
  skipMany white
  ip <- try ipv4 <|> ipv6
  skipSome white
  names <- hostname `sepEndBy1` many white
  void $ optional $ try comment
  return (ip, names)


num :: Parser String
num = choice [ string "0", nonzero ]


nonzero :: Parser String
nonzero = do
  a <- oneOf ['1'..'9'] <?> "non-zero digit with non-zero part"
  rest <- count' 0 2 digitChar
  if (read (a:rest) :: Int) > 255
    then fail "IP address parts must be 0 <= x <= 255"
    else return (a:rest)


-- TODO: check RFC
hostname :: Parser Name
hostname = Name <$> some (alphaNumChar <|> oneOf ['.', '-'])


dot :: Parser ()
dot = void $ char '.'


ipv4 :: Parser IP
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


ipv6 :: Parser IP
ipv6 = IP <$> some (hexDigitChar <|> char ':') <?> "ipv6 address"


white :: Parser Char
white = oneOf [' ', '\t']
