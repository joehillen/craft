module Craft.Ssh.PrivateKey where

import           Craft
import           Craft.Ssh
import qualified Craft.Directory as Directory
import           Craft.File (file)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.User (User)
import qualified Craft.User as User
import qualified Craft.Group as Group
import qualified Data.ByteString.Char8 as B8

import Control.Lens

data PrivateKey
  = PrivateKey
    { user :: User
    , name :: String
    , content :: String
    }
  deriving (Eq, Show)

path :: PrivateKey -> FilePath
path PrivateKey{..} = Directory.path (userDir user) </> name

instance Craftable PrivateKey where
  watchCraft pk = do
    craft_ $ userDir $ user pk
    w <- watchCraft_ $ file (Craft.Ssh.PrivateKey.path pk)
                         & File.mode       .~ Mode RW O O
                         & File.ownerID    .~ User.uid (user pk)
                         & File.groupID    .~ Group.gid (User.group $ user pk)
                         & File.strContent .~ content pk
    return (w, pk)
