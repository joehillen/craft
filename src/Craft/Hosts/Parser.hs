module Craft.Hosts.Parser where

-- |This file is just to keep Megaparsec from conflicting with other modules

import Control.Monad (void)
import Data.List (intercalate)
import Text.Megaparsec
import Text.Megaparsec.String

import Craft.Hosts.Types
import Craft.Types
import Craft.Internal.Helpers.Parsing


parseLine :: Int -> String -> Craft (Maybe (IP, [Name]))
parseLine ln s =
  case runParser lineParser (fromAbsFile hostsfp) s of
    Right x  -> return x
    Left err -> $craftError $ show err
 where
  lineParser :: Parser (Maybe (IP, [Name]))
  lineParser = do
    pos <- getPosition
    setPosition $ pos {sourceLine = ln}
    space
    try (comment >> return Nothing) <|> try (Just <$> item)
                                    <|> (end >> return Nothing)


comment :: Parser ()
comment = label "comment" $ do
  void $ char '#'
  void $ manyTill (noneOf "\r\n") end
  return ()


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
aliases = label "aliases" $ hostname `sepEndBy` many white


num :: Parser String
num = choice [ string "0", nonzero ]


nonzero :: Parser String
nonzero = do
  a <- oneOf ['1'..'9'] <?> "non-zero digit with non-zero part"
  rest <- count' 0 2 digitChar
  if (read (a:rest) :: Int) > 255
    then fail "IP address parts must be 0 <= x <= 255"
    else return (a:rest)


hostname :: Parser Name
hostname = Name <$> some (alphaNumChar <|> oneOf ".-")


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
