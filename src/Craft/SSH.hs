module Craft.SSH where

import Control.Lens

import           Craft
import           Craft.Directory (Directory, Directory(Directory))
import           Craft.File
import           Craft.File.Mode
import           Craft.User (User)
import qualified Craft.User as User


data PublicKey
  = PublicKey
    { _publicKey     :: String
    , _publicKeyType :: String
    }
makeLenses ''PublicKey

addAuthorizedKey :: User -> PublicKey -> Craft File
addAuthorizedKey user pk = do
  craft_ $ userDir user
  craft $
    file (user ^. User.home </> ".ssh" </> "authorized_keys")
         & mode       .~ Mode RW O O
         & ownerID    .~ user ^. User.uid
         & groupID    .~ user ^. User.gid
         & strContent .~ pk ^. publicKeyType ++ " " ++ pk ^. publicKey ++ "\n"


userDir :: User -> Directory
userDir user =
  Directory (user ^. User.home </> ".ssh")
            (Mode RWX O O)
            (user ^. User.uid)
            (user ^. User.gid)
