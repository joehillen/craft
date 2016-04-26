module Craft.Internal.FileDirectory
( module Craft.Internal.FileDirectory
, FilePath
) where

import Craft.File.Mode (Mode(..), fromString)
import Craft.Group (GroupID(..))
import Craft.Internal
import Craft.User (UserID(..))

import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.String


setOwnerID :: UserID -> FilePath -> Craft ()
setOwnerID uid path = exec_ "/bin/chown" [show uid, path]


setGroupID :: GroupID -> FilePath -> Craft ()
setGroupID gid path = exec_ "/bin/chgrp" [show gid, path]


stat :: Args -> Craft ExecResult
stat = exec "stat"

getMode :: FilePath -> Craft Mode
getMode fp = do
  r <- stat ["-c", "%a", fp]
  success <- $errorOnFail r
  parseExecResult r modeParser (success ^. stdout)


getOwnerID :: FilePath -> Craft UserID
getOwnerID fp = do
  r <- stat ["-c", "%u", fp]
  success <- $errorOnFail r
  UserID <$> parseExecResult r digitParser (success ^. stdout)


getGroupID :: FilePath -> Craft GroupID
getGroupID fp = do
  r <- stat ["-c", "%g", fp]
  success <- $errorOnFail r
  GroupID <$> parseExecResult r digitParser (success ^. stdout)


getStats :: FilePath -> Craft (Maybe (Mode, UserID, GroupID))
getStats fp =
  stat ["-c", "%a:%u:%g", fp] >>= \case
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
modeParser = fromString <$> some digitChar


digitParser :: Parser Int
digitParser = read <$> some digitChar
