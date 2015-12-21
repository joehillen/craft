module Craft.Hosts
( IP(..)
, Name(..)
, Configs
, Hosts(..)
, Craft.Hosts.lookup
, hostsfp
, get
, parse
, showConfigs
, toFile
, insert
, deleteIP
, deleteName
, delete
, set
)
where


import qualified Data.ByteString.Char8 as B8
import Data.List (intercalate)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Text.Megaparsec hiding (parse, fromFile)
import Text.Megaparsec.String

import Craft.Internal
import Craft.File (File)
import qualified Craft.File as File


newtype IP = IP String
  deriving (Eq)

instance Show IP where
  show (IP ip) = ip

newtype Name = Name String
  deriving (Eq)

instance Show Name where
  show (Name name) = name

type Configs = [(IP, [Name])]
data Hosts = Hosts { configs :: Configs }
             deriving (Eq, Show)

lookup :: IP -> Hosts -> Maybe [Name]
lookup ip hosts = L.lookup ip $ configs hosts


hostsMap :: ((IP, [Name]) -> (IP, [Name])) -> Hosts -> Hosts
hostsMap f (Hosts cfgs) = Hosts $ Prelude.map f cfgs


hostsfp :: FilePath
hostsfp = "/etc/hosts"


get :: Craft Hosts
get =
  File.get hostsfp >>= \case
    Nothing -> $craftError $ hostsfp ++ " not found!"
    Just f  -> case File.content f of
                 Nothing -> $craftError $ hostsfp ++ " not found!"
                 Just bs -> return . parse $ B8.unpack bs


parse :: String -> Hosts
parse = Hosts . catMaybes . zipWith parseLine [1..] . lines


instance Craftable Hosts where
  watchCraft hosts = do
    (w, f) <- watchCraft $ toFile hosts
    return (w, fromFile f)


showConfigs :: Configs -> String
showConfigs = unlines
            . map (\(ip, as) -> unwords (show ip:map show as))


toFile :: Hosts -> File
toFile (Hosts cfgs) =
  (File.file hostsfp)
     {File.content = Just . B8.pack $ showConfigs cfgs}


fromFile :: File -> Hosts
fromFile f =
  case File.content f of
    Nothing -> Hosts []
    Just c  -> parse $ B8.unpack c


insert :: IP -> Name -> Hosts -> Hosts
insert newip name (Hosts cfgs) = fixUp $ Hosts go
 where
  go | any ((== newip) . fst) cfgs = map f cfgs
     | otherwise           = cfgs ++ [(newip, [name])]
  f (ip, names) | ip == newip && name `notElem` names = (ip, name:names)
                | otherwise                           = (ip, names)


deleteIP :: IP -> Hosts -> Hosts
deleteIP ip (Hosts cfgs) = fixUp . Hosts $ filter (\(ip', _) -> ip' /= ip) cfgs


deleteName :: Name -> Hosts -> Hosts
deleteName name = fixUp . hostsMap f
 where
   f (ip', names) = (ip', filter (/= name) names)


delete :: IP -> Name -> Hosts -> Hosts
delete ip name = fixUp . hostsMap f
 where
  f (ip', names) | ip' == ip = (ip', filter (/= name) names)
                 | otherwise = (ip', names)


set :: Name -> IP -> Hosts -> Hosts
set name ip hosts = fixUp . insert ip name $ deleteName name hosts

----------------------------------------
--   ____       _            _        --
--  |  _ \ _ __(_)_   ____ _| |_ ___  --
--  | |_) | '__| \ \ / / _` | __/ _ \ --
--  |  __/| |  | |\ V / (_| | ||  __/ --
--  |_|   |_|  |_| \_/ \__,_|\__\___| --
----------------------------------------
fixUp :: Hosts -> Hosts
fixUp (Hosts cfgs) = Hosts $ filter f  cfgs
 where
  f (IP ip, names) = not (null ip) && not (null names)


parseLine :: Int -> String -> Maybe (IP, [Name])
parseLine ln s =
  case runParser lineParser hostsfp s of
    Right x  -> x
    Left err -> error $ show err
 where
  lineParser :: Parser (Maybe (IP, [Name]))
  lineParser = do
    pos <- getPosition
    setPosition $ pos {sourceLine = ln}
    space
    try (comment >> return Nothing) <|> try (Just <$> item)
                                    <|> (eof >> return Nothing)


comment :: Parser ()
comment = label "comment" $ do
  void $ char '#'
  void $ manyTill (noneOf "\r\n") $ try (void eol) <|> eof
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
