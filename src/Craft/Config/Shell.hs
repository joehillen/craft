module Craft.Config.Shell where

import           Control.Lens hiding (set, noneOf)
import           Data.Maybe (catMaybes)
import           Text.Megaparsec hiding (parse)

import           Craft hiding (try)
import           Craft.Config
import           Craft.Internal.Helpers.Parsing


newtype ShellFormat = ShellFormat { _shellfmt :: [(String, String)] }
  deriving (Eq, Show)
makeLenses ''ShellFormat


instance ConfigFormat ShellFormat where
  showConfig (ShellFormat cfgs) = unlines $ map showkv cfgs
   where
     showkv :: (String, String) -> String
     showkv (k, v) = k++"="++v
  parse fp s =
    case runParser parser (show fp) s of
      Left err   -> $craftError $ show err
      Right cfgs -> return cfgs

-- | Replace an existing value if it is already set otherwise add it to the end.
set :: String -> String -> ShellFormat -> ShellFormat
set k v (ShellFormat cfgs) =
  case lookup k cfgs of
    Nothing -> ShellFormat $ cfgs++[(k,v)]
    Just _  -> ShellFormat $ map (replace k v) cfgs
 where
  replace ka va (kb, vb) | ka == kb  = (ka, va)
                         | otherwise = (kb, vb)


unset :: String -> ShellFormat -> ShellFormat
unset k (ShellFormat cfgs) = ShellFormat $ filter ((/= k) . fst) cfgs


get :: Path Abs FileP -> Craft (Maybe (Config ShellFormat))
get = Craft.Config.get


config :: Path Abs FileP -> [(String, String)] -> Config ShellFormat
config fp = Craft.Config.config fp . ShellFormat


dedup :: ShellFormat -> ShellFormat
dedup cfgs = go empty cfgs
 where
  go m (ShellFormat          [])   = m
  go m (ShellFormat ((k, v):xs)) = go (set k v m) (ShellFormat xs)


empty :: ShellFormat
empty = ShellFormat []


-- TESTME
parser :: Parsec String ShellFormat
parser = do
  items <- line `sepEndBy` eol
  return . ShellFormat $ catMaybes items


line :: Parsec String (Maybe (String, String))
line =  do
  try (comment >> return Nothing)
  <|> try (space >> end >> return Nothing)
  <|> (Just <$> item)


comment :: Parsec String ()
comment = space >> char '#' >> manyTill anyChar end >> return ()


item :: Parsec String (String, String)
item = do
  space
  name <- some $ noneOf " ="
  void $ char '='
  -- TODO: quoted values
  value <- try (someTill (noneOf " \r\n#") (lookAhead (oneOf " \r\n#")) <* manyTill anyChar end)
        <|> (many (oneOf " \t") >> manyTill anyChar end >> return "")
  return (name, value)
