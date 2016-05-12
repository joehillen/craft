module Craft.SSH.PrivateKey where

import           Control.Lens

import           Craft
import qualified Craft.Directory as Dir
import           Craft.File (file)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.SSH
import           Craft.User (User)
import qualified Craft.User as User


data PrivateKey
  = PrivateKey
    { _user    :: User
    , _name    :: String
    , _content :: String
    }
  deriving (Eq, Show)
makeLenses ''PrivateKey


path :: Getter PrivateKey FilePath
path = to $ \pk -> userDir (pk ^. user) ^. Dir.path </> (pk ^. name)


instance Craftable PrivateKey PrivateKey where
  watchCraft pk = do
    craft_ $ userDir $ pk ^. user
    w <- watchCraft_ $ file (pk ^. path)
                       & File.mode          .~ Mode RW O O
                       & File.ownerAndGroup .~ pk ^. user
                       & File.strContent    .~ pk ^. content
    return (w, pk)
