module Craft.Internal.FileDirectory
( module Craft.Internal.FileDirectory
, FilePath
) where

import Craft.File.Mode (Mode(..), fromOctalString)
import Craft.Group (GroupID(..))
import Craft.Internal
import Craft.User (UserID(..))

import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.String


setOwnerID :: UserID -> FilePath -> Craft ()
setOwnerID (UserID uid) path = exec_ "/bin/chown" [show uid, path]


setGroupID :: GroupID -> FilePath -> Craft ()
setGroupID (GroupID gid) path = exec_ "/bin/chgrp" [show gid, path]


stat :: Command
stat = "stat"

getMode :: FilePath -> Craft Mode
getMode fp = parseExecStdout modeParser stat ["-c", "%a", fp]


getOwnerID :: FilePath -> Craft UserID
getOwnerID fp = UserID <$> parseExecStdout digitParser stat ["-c", "%u", fp]


getGroupID :: FilePath -> Craft GroupID
getGroupID fp = GroupID <$> parseExecStdout digitParser stat ["-c", "%g", fp]


getStats :: FilePath -> Craft (Maybe (Mode, UserID, GroupID))
getStats fp =
  exec stat ["-c", "%a:%u:%g", fp] >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> Just <$> parseExecResult (ExecSucc r) statsParser (r ^. stdout)


statsParser :: Parser (Mode, UserID, GroupID)
statsParser = do
  mode <- modeParser
  void $ char ':'
  owner <- UserID <$> digitParser
  void $ char ':'
  group <- GroupID <$> digitParser
  return (mode, owner, group)


modeParser :: Parser Mode
modeParser = fromOctalString <$> some digitChar


digitParser :: Parser Int
digitParser = read <$> some digitChar
