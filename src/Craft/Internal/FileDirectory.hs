module Craft.Internal.FileDirectory
( module Craft.Internal.FileDirectory
, FilePath
) where

import           Craft.Internal
import           Craft.File.Mode (Mode(..), fromString)
import           Craft.Group (GroupID)
import           Craft.User (UserID)

import           Text.Megaparsec
import           Text.Megaparsec.String


setOwnerID :: UserID -> FilePath -> Craft ()
setOwnerID uid path = exec_ "/bin/chown" [show uid, path]

setGroupID :: GroupID -> FilePath -> Craft ()
setGroupID gid path = exec_ "/bin/chgrp" [show gid, path]

getMode :: FilePath -> Craft Mode
getMode fp = fromString . stdout . errorOnFail
             <$> exec "/usr/bin/stat" ["-c", "%a", fp]

digitParser :: Parser Int
digitParser = read <$> some digitChar

getOwnerID :: FilePath -> Craft UserID
getOwnerID fp = parseExec digitParser stdout "/usr/bin/stat" ["-c", "%u", fp]

getGroupID :: FilePath -> Craft GroupID
getGroupID fp = parseExec digitParser stdout "/usr/bin/stat" ["-c", "%g", fp]
