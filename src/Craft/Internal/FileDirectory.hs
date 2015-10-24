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

import           Text.Megaparsec
import           Text.Megaparsec.String


setOwner :: FilePath -> User -> Craft ()
setOwner path User{..} = exec_ "/bin/chown" [show uid, path]

setGroup :: FilePath -> Group -> Craft ()
setGroup path Group{..} = exec_ "/bin/chgrp" [show gid, path]

getMode :: FilePath -> Craft Mode
getMode p = fromString . stdout . errorOnFail
            <$> exec "/usr/bin/stat" ["-c", "%a", p]

digitParser :: Parser Int
digitParser = read <$> some digitChar

getOwner :: FilePath -> Craft User
getOwner p = do
  uid <- parseExec digitParser stdout "/usr/bin/stat" ["-c", "%u", p]
  User.fromID uid >>= \case
    Nothing -> error $ "No such owner with id `" ++ show uid ++ "` for: " ++ p
    Just g  -> return g

getGroup :: FilePath -> Craft Group
getGroup p = do
  gid <- parseExec digitParser stdout "/usr/bin/stat" ["-c", "%g", p]
  Group.fromID gid >>= \case
    Nothing -> error $
      "No such group with id `" ++ show gid ++ "` for: " ++ p
    Just g -> return g
