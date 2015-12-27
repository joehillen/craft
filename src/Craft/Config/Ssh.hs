module Craft.Config.Ssh where

import Control.Lens
import           Data.Char (toLower)
import           Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B8
import           Text.Megaparsec

import           Craft
import qualified Craft.Directory as Directory
import           Craft.File (File, file)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.Internal.Helpers
import           Craft.Ssh
import           Craft.User (User, UserID)
import qualified Craft.User as User
import           Craft.Group (GroupID)
import qualified Craft.Group as Group


data UserConfig
  = UserConfig
    { user        :: User
    , userConfigs :: [Section]
    }
  deriving Eq


userPath :: UserConfig -> FilePath
userPath UserConfig{..} = userDir user ^. Directory.path </> "config"

data Config
  = Config
    { path    :: FilePath
    , mode    :: Mode
    , ownerID :: UserID
    , groupID :: GroupID
    , configs :: [Section]
    }
    deriving (Eq)


instance Show Config where
  show f = "Ssh.Config " ++
           "{ path = " ++ show (path f) ++
           ", mode = " ++ show (mode f) ++
           ", ownerID = " ++ show (ownerID f) ++
           ", groupID = " ++ show (groupID f) ++
           ", configs = \"" ++ show (configs f) ++ "\"" ++
           "}"


config :: FilePath -> [Section] -> Craft Config
config fp cfgs = do
  f <- configFromFile $ File.file fp
  return f { configs = cfgs }


configFromFile :: File -> Craft Config
configFromFile f = do
  cfgs <- case f ^. File.content of
            Nothing -> return []
            Just bs -> Craft.Config.Ssh.parse (f ^. File.path) (B8.unpack bs)
  return Config { path    = f ^. File.path
                , mode    = f ^. File.mode
                , ownerID = f ^. File.ownerID
                , groupID = f ^. File.groupID
                , configs = cfgs
                }

fileFromConfig :: Config -> File
fileFromConfig cfg =
  File.file (path cfg) & File.mode    .~ mode cfg
                       & File.ownerID .~ ownerID cfg
                       & File.groupID .~ groupID cfg
                       & File.strContent .~ show (configs cfg)



get :: FilePath -> Craft (Maybe Config)
get fp =
  File.get fp >>= \case
    Nothing -> return Nothing
    Just f  -> Just <$> configFromFile f


data Section
  = Host  String Body
  | Match String Body
  deriving Eq


type Sections = [Section]


type Body = [(String, String)]


bodyLookup :: String -> Body -> Maybe String
bodyLookup key body =
  lookup (map toLower key) $ map ((,) <$> map toLower . fst <*> snd) body


cfgLookup :: String -> String -> Sections -> Maybe String
cfgLookup sectionName key sections' =
  case body of
    Just b -> bodyLookup key b
    Nothing -> Nothing
 where
  body = maybeHead . catMaybes $ map f sections'
  f (Host name b) | name == sectionName = Just b
                  | otherwise           = Nothing
  f             _                       = Nothing
  maybeHead []    = Nothing
  maybeHead (x:_) = Just x


instance Craftable UserConfig where
  watchCraft cfg = do
    craft_ $ userDir $ user cfg
    w <- watchCraft_ $ file (userPath cfg)
                         & File.mode .~ Mode RW O O
                         & File.ownerID .~ User.uid (user cfg)
                         & File.groupID .~ Group.gid (User.group $ user cfg)
                         & File.strContent .~ show cfg
    return (w, cfg)


parse :: String -> String -> Craft [Section]
parse fp s =
  case runParser parser fp s of
    Left err       -> $craftError $ show err
    Right sections -> return sections


instance Show UserConfig where
  show UserConfig{..} = unlines $ fmap show userConfigs


showBody :: Body -> String
showBody body = indent 4 . unlines $ map (\(n, v) -> n ++ " " ++ v) body


instance Show Section where
  show (Host hostname body)  = "Host " ++ hostname ++ "\n" ++ showBody body
  show (Match match body)    = "Match " ++ match ++ "\n" ++ showBody body


parseTitle :: Parsec String (String, String)
parseTitle = do
  space
  type' <- try (string' "host") <|> string' "match"
  void spaceChar
  notFollowedBy eol
  space
  name <- someTill anyChar eol
  return (type', name)


parseBody :: Parsec String [(String, String)]
parseBody = many bodyLine


bodyLine :: Parsec String (String, String)
bodyLine = do
  notFollowedBy parseTitle
  space
  name <- someTill anyChar spaceChar
  notFollowedBy eol
  space
  val <- someTill anyChar (void eol <|> eof)
  void $ optional (space <|> void (many eol))
  return (name, trim val)


parseSection :: Parsec String Section
parseSection = do
  title <- parseTitle
  body <- parseBody
  return $ case map toLower (fst title) of
    "host"  -> Host (snd title) body
    "match" -> Match (snd title) body
    _       -> error "Craft.Config.Ssh.parse failed. This should be impossible."


parser :: Parsec String [Section]
parser = some parseSection
