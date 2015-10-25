module Craft.Ssh.PrivateKey where

import           Craft
import           Craft.Ssh
import qualified Craft.Directory as Directory
import           Craft.File (File(File))
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.User (User)
import qualified Craft.User as User

data PrivateKey
  = PrivateKey
    { user :: User
    , name :: String
    , content :: String
    }
  deriving (Eq, Show)

path :: PrivateKey -> File.Path
path PrivateKey{..} = Directory.path (userDir user) </> name

instance Craftable PrivateKey where
  checker pk = File.get (path pk) >>= \case
    Nothing -> return Nothing
    Just  f -> return . Just $ pk { content = File.contentAsString f }

  crafter pk@PrivateKey{..} = do
    craft_ $ userDir user
    craft_ $ File (Craft.Ssh.PrivateKey.path pk)
                   (Mode RW O O)
                   (Just user)
                   (Just $ User.group user)
                   (File.strContent content)

  destroyer = notImplemented "destroyer PrivateKey"
