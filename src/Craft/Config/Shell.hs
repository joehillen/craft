module Craft.Config.Shell where

import           Control.Lens
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Text.Megaparsec hiding (parse)

import           Craft hiding (try)
import           Craft.Config
import           Craft.Internal.Helpers


newtype ShellFormat = ShellFormat { _shellfmt :: Map String String }

fromList :: [(String, String)] -> ShellFormat
fromList = ShellFormat . M.fromList

toList :: ShellFormat -> [(String, String)]
toList = M.toList . _shellfmt


instance ConfigFormat ShellFormat where
  showConfig cfgs = intercalate "\n" (map showkv $ M.toList (_shellfmt cfgs)) ++ "\n"
   where
     showkv :: (String, String) -> String
     showkv (k, v) = k ++ "=" ++ v
  parse fp s =
    case runParser parser fp s of
      Left err   -> $craftError $ show err
      Right cfgs -> return cfgs


get :: FilePath -> Craft (Maybe (Config ShellFormat))
get = Craft.Config.get


-- TESTME
parser :: Parsec String ShellFormat
parser = do
  items <- many line
  return . ShellFormat . M.fromList $ catMaybes items


line :: Parsec String (Maybe (String, String))
line = do
  space
  try (comment >> return Nothing) <|> (Just <$> item)


var :: Parsec String String
var = some (alphaNumChar <|> char '_')


comment :: Parsec String ()
comment = char '#' >> manyTill anyChar eol >> return ()


item :: Parsec String (String, String)
item = do
  space
  name <- var
  space
  void $ char '='
  space
  val <- manyTill anyChar (try comment <|> end)
  return (name, trim val)


makeLenses ''ShellFormat
