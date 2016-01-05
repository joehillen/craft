module Craft.Config.Shell where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Text.Megaparsec hiding (parse)
import Data.List (intercalate)

import Craft
import Craft.Config
import Craft.Internal.Helpers


newtype ShellFormat = ShellFormat { _shellfmt :: Map Text Text }



instance ConfigFormat ShellFormat where
  showConfig cfgs = intercalate "\n" (map showkv $ M.toList (_shellfmt cfgs))
                 ++ "\n"
   where
     showkv :: (Text, Text) -> Text
     showkv (k, v) = k ++ "=" ++ v
  parse fp s =
    case runParser parser fp s of
      Left err   -> $craftError $ show err
      Right cfgs -> return cfgs


get :: FilePath -> Craft (Maybe (Config ShellFormat))
get = Craft.Config.get


-- TESTME
parser :: Parsec Text ShellFormat
parser = do
  items <- many line
  return . ShellFormat . M.fromList $ catMaybes items


line :: Parsec Text (Maybe (Text, Text))
line = do
  space
  try (comment >> return Nothing) <|> (Just <$> item)


var :: Parsec Text Text
var = some (alphaNumChar <|> char '_')


comment :: Parsec Text ()
comment = char '#' >> manyTill anyChar eol >> return ()


item :: Parsec Text (Text, Text)
item = do
  space
  name <- var
  space
  void $ char '='
  space
  val <- manyTill anyChar (try comment <|> end)
  return (name, trim val)


makeLenses ''ShellFormat
