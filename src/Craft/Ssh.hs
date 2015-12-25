module Craft.Ssh where

import Control.Lens

import           Craft
import           Craft.Directory (Directory, Directory(Directory))
import           Craft.File
import           Craft.File.Mode
import           Craft.User (User)
import qualified Craft.User as User
import qualified Craft.Group as Group


data PublicKey
  = PublicKey
    { publicKey     :: String
    , publicKeyType :: String
    }

addAuthorizedKey :: User -> PublicKey -> Craft File
addAuthorizedKey user pk = do
  craft_ $ userDir user
  craft $
    file (User.home user </> ".ssh" </> "authorized_keys")
         & mode       .~ Mode RW O O
         & ownerID    .~ User.uid user
         & groupID    .~ Group.gid (User.group user)
         & strContent .~ publicKeyType pk ++ " " ++ publicKey pk

userDir :: User -> Directory
userDir user =
  Directory (User.home user </> ".ssh")
            (Mode RWX O O)
            (User.uid user)
            (Group.gid $ User.group user)

