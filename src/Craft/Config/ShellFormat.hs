module Craft.Config.ShellFormat where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Text.Megaparsec hiding (parse)
import Data.List (intercalate)
import qualified Data.ByteString.Char8 as B8

import Craft
import Craft.Internal.Helpers
import Craft.File (File)
import qualified Craft.File as File
import Craft.File.Mode
import Craft.User (UserID)
import Craft.Group (GroupID)


data Config
  = Config
    { path    :: FilePath
    , mode    :: Mode
    , ownerID :: UserID
    , groupID :: GroupID
    , configs :: Configs
    }
    deriving (Eq)


type Configs = Map String String


showConfigs :: Configs -> String
showConfigs cfgs =
  intercalate "\n" (map (\(k, v) -> k ++ "=" ++ v) $ M.toList cfgs)
  ++ "\n"


instance Show Config where
  show f = "ShellFormat.Config " ++
           "{ path = " ++ show (path f) ++
           ", mode = " ++ show (mode f) ++
           ", ownerID = " ++ show (ownerID f) ++
           ", groupID = " ++ show (groupID f) ++
           ", configs = \"" ++ showConfigs (configs f) ++ "\"" ++
           "}"


config :: FilePath -> Configs -> Craft Config
config fp cfgs = do
  f <- configFromFile $ File.file fp
  return f { configs = cfgs }


configFromFile :: File -> Craft Config
configFromFile f = do
  cfgs <- case f ^. File.content of
            Nothing -> return M.empty
            Just bs -> parse (f ^. File.path) (B8.unpack bs)
  return Config { path    = f ^. File.path
                , mode    = f ^. File.mode
                , ownerID = f ^. File.ownerID
                , groupID = f ^. File.groupID
                , configs = cfgs
                }

fileFromConfig :: Config -> File
fileFromConfig cfg =
  File.file (path cfg) & File.mode       .~ mode cfg
                       & File.ownerID    .~ ownerID cfg
                       & File.groupID    .~ groupID cfg
                       & File.strContent .~ showConfigs (configs cfg)


get :: FilePath -> Craft (Maybe Config)
get fp =
  File.get fp >>= \case
    Nothing -> return Nothing
    Just f  -> Just <$> configFromFile f


instance Craftable Config where
  watchCraft cfg = do
    w <- watchCraft_ $ fileFromConfig cfg
    return (w, cfg)


-- TESTME
parser :: Parsec String Configs
parser = do
  items <- many line
  return . M.fromList $ catMaybes items


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


parse :: FilePath -> String -> Craft Configs
parse fp s =
  case runParser parser fp s of
    Left err   -> $craftError $ show err
    Right cfgs -> return cfgs
