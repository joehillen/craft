module Craft.Hosts.Parser where

-- |This file is just to keep Megaparsec from conflicting with other modules

import           Control.Monad                  (void)
import           Data.List                      (intercalate)
import           Data.Maybe                     (catMaybes)
import           Text.Megaparsec
-- import           Text.Megaparsec.Lexer
import           Text.Megaparsec.String

import           Craft.Hosts.Types
import           Craft.Internal.Helpers.Parsing
import           Craft.Types


parseHosts :: String -> Craft Hosts
parseHosts s = do
  case runParser parser hostsfp s of
    Right x  -> return x
    Left err -> $craftError $ show err


parser :: Parser Hosts
parser = do
  rs <- parserLine `sepEndBy` eol
  return . Hosts $ catMaybes rs


parserLine :: Parser (Maybe (IP, [Name]))
parserLine = do
  -- try (Just <$> ((,) <$> pure (IP "comment") <*> ((:[]) . Name <$> comment)))
  try (comment >> return Nothing)
  <|> try (end >> return Nothing)
  <|> (Just <$> item)


comment :: Parser String
comment = do
  void $ char '#'
  manyTill anyChar end


item :: Parser (IP, [Name])
item = do
  ip <- try ipv4 <|> ipv6
  skipSome white
  name <- hostname
  skipMany white
  as <- aliases
  void $ optional comment
  return (ip, name:as)


aliases :: Parser [Name]
aliases =
  label "aliases" $
    hostname `sepEndBy` many (oneOf [' ', '\t'])


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
  p1 <- nonzero
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


white :: Parser ()
white = void $ (char ' ' <?> "space character") <|> tab
