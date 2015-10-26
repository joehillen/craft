module Craft.Ssh where

import           Craft
import           Craft.Directory (Directory, Directory(Directory))
import           Craft.File (File, File(..))
import qualified Craft.File as File
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
    File (User.home user </> ".ssh" </> "authorized_keys")
         (Mode RW O O)
         (User.uid user)
         (Group.gid $ User.group user)
         (File.strContent $ publicKeyType pk ++ " " ++ publicKey pk)

userDir :: User -> Directory
userDir user =
  Directory (User.home user </> ".ssh")
            (Mode RWX O O)
            (User.uid user)
            (Group.gid $ User.group user)

