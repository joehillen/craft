module Craft.Ssh.Config where

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
      cfg { sections = parse $ File.contentAsString f }

  crafter config@Config{..} = do
    craft_ $ userDir user
    craft_ $
      File (path config)
           (Mode RW O O)
           user
           (User.group user)
           (File.strContent $ show config)

  remover = notImplemented "checker Ssh.Config"

parse :: String -> [Section]
parse = notImplemented "Ssh.Config.parse"

instance Show Config where
  show Config{..} = unlines $ fmap show sections

showBody :: Body -> String
showBody body = indent 4 . unlines $ map (\(n, v) -> n ++ " " ++ v) body

instance Show Section where
  show (Host hostname body)  = "Host " ++ hostname ++ "\n" ++ showBody body
  show (Match match body)    = "Match " ++ match ++ "\n" ++ showBody body
