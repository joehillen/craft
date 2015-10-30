module Craft.Config.Ssh where

import           Control.Monad (void)
import           Data.Char (toLower)
import           Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B8
import           Text.Megaparsec

import           Craft
import qualified Craft.Directory as Directory
import           Craft.File (File(File))
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


userPath :: UserConfig -> File.Path
userPath UserConfig{..} = Directory.path (userDir user) </> "config"

data Config
  = Config
    { path    :: File.Path
    , mode    :: Mode
    , ownerID :: UserID
    , groupID :: GroupID
    , configs :: [Section]
    }
    deriving (Eq)


instance Show Config where
  show f = "Config { path = " ++ show (path f) ++
                  ", mode = " ++ show (mode f) ++
                  ", ownerID = " ++ show (ownerID f) ++
                  ", groupID = " ++ show (groupID f) ++
                  ", configs = \"" ++ show (configs f) ++ "\"" ++
                  "}"


config :: File.Path -> [Section] -> Config
config fp cfgs = (configFromFile $ File.file fp) { configs = cfgs }


configFromFile :: File -> Config
configFromFile f =
  Config { path    = File.path f
         , mode    = File.mode f
         , ownerID = File.ownerID f
         , groupID = File.groupID f
         , configs = case File.content f of
                       Nothing -> []
                       Just bs -> Craft.Config.Ssh.parse (File.path f)
                                                         (B8.unpack bs)
         }

fileFromConfig :: Config -> File
fileFromConfig cfg =
  File.File { File.path    = path cfg
            , File.mode    = mode cfg
            , File.ownerID = ownerID cfg
            , File.groupID = groupID cfg
            , File.content = Just . B8.pack . show $ configs cfg
            }


get :: File.Path -> Craft (Maybe Config)
get fp = fmap configFromFile <$> File.get fp


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
  checker cfg = File.get (userPath cfg) >>= \case
    Nothing -> return Nothing
    Just  f -> return . Just $
      cfg { userConfigs = Craft.Config.Ssh.parse
                             (userPath cfg)
                             (File.contentAsString f)
          }

  crafter config@UserConfig{..} _ = do
    craft_ $ userDir user
    craft_ $
      File (userPath config)
           (Mode RW O O)
           (User.uid user)
           (Group.gid $ User.group user)
           (File.strContent $ show config)

  destroyer = notImplemented "destroyer Ssh.UserConfig"


parse :: String -> String -> [Section]
parse fp s =
  case runParser parser fp s of
    Left err       -> error $ show err
    Right sections -> sections


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
  void $ spaceChar
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
  void $ optional $ (space <|> void (many eol))
  return (name, trim val)


parseSection :: Parsec String Section
parseSection = do
  title <- parseTitle
  body <- parseBody
  return $ case map toLower (fst title) of
    "host"  -> Host (snd title) body
    "match" -> Match (snd title) body
    _       -> error $ "Craft.Config.Ssh.parse failed. " ++
                       "This should be impossible."


parser :: Parsec String [Section]
parser = some parseSection
