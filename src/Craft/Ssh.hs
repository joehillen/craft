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
    file (user ^. User.home </> ".ssh" </> "authorized_keys")
         & mode       .~ Mode RW O O
         & ownerID    .~ user ^. User.uid
         & groupID    .~ user ^. User.gid
         & strContent .~ publicKeyType pk ++ " " ++ publicKey pk

userDir :: User -> Directory
userDir user =
  Directory (user ^. User.home </> ".ssh")
            (Mode RWX O O)
            (user ^. User.uid)
            (user ^. User.gid)
