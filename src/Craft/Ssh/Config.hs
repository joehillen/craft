module Craft.Ssh.Config where

import           Control.Monad (void)
import           Data.Char (toLower)
import           Text.Megaparsec

import           Craft
import qualified Craft.Directory as Directory
import           Craft.File (File(File))
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.Internal.Helpers
import           Craft.Ssh
import           Craft.User (User)
import qualified Craft.User as User


data Config
  = Config
    { user :: User
    , sections :: [Section]
    }
  deriving Eq

path :: Config -> File.Path
path Config{..} = Directory.path (userDir user) </> "config"

data Section
  = Host  String Body
  | Match String Body
  deriving Eq

type Body = [(String, String)]

instance Craftable Config where
  checker cfg = File.get (path cfg) >>= \case
    Nothing -> return Nothing
    Just  f -> return . Just $
      cfg { sections = Craft.Ssh.Config.parse
                         (path cfg)
                         (File.contentAsString f)
          }

  crafter config@Config{..} = do
    craft_ $ userDir user
    craft_ $
      File (path config)
           (Mode RW O O)
           (Just user)
           (Just $ User.group user)
           (File.strContent $ show config)

  remover = notImplemented "remover Ssh.Config"

parse :: String -> String -> [Section]
parse fp s =
  case runParser parser fp s of
    Left err       -> error $ show err
    Right sections -> sections

instance Show Config where
  show Config{..} = unlines $ fmap show sections

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
    _       -> error "Craft.Ssh.Config.parse failed. This should be impossible."


parser :: Parsec String [Section]
parser = some parseSection
