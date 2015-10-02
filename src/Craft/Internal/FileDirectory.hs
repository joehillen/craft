module Craft.Internal.FileDirectory
( module Craft.Internal.FileDirectory
, FilePath
) where

import           Craft
import           Craft.File.Mode (Mode(..), fromString)
import           Craft.Group (Group(..))
import qualified Craft.Group as Group
import           Craft.User (User(..))
import qualified Craft.User as User

import           Text.Parsec


setOwner :: User -> FilePath -> Craft ()
setOwner Root path = exec_ "/bin/chown" ["0", path]
setOwner User{..} path = exec_ "/bin/chown" [show uid, path]

setGroup :: Group -> FilePath -> Craft ()
setGroup RootGroup path = exec_ "/bin/chgrp" ["0", path]
setGroup Group{..} path = exec_ "/bin/chgrp" [show gid, path]

getMode :: FilePath -> Craft Mode
getMode p = fromString . stdout <$> exec "/usr/bin/stat" ["-c", "%a", p]

getOwner :: FilePath -> Craft User
getOwner p = do
  uid <- parseExec (read <$> many1 digit) "/usr/bin/stat" ["-c", "%u", p]
  User.fromID uid >>= \case
    Nothing -> error $ "No such owner with id `" ++ show uid ++ "` for: " ++ p
    Just g  -> return g

getGroup :: FilePath -> Craft Group
getGroup p = do
  gid <- parseExec (read <$> many1 digit) "/usr/bin/stat" ["-c", "%g", p]
  Group.fromID gid >>= \case
    Nothing -> error $
      "No such group with id `" ++ show gid ++ "` for: " ++ p
    Just g -> return g
