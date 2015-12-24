module Craft.Internal.FileDirectory
( module Craft.Internal.FileDirectory
, FilePath
) where

import           Craft.Internal
import           Craft.File.Mode (Mode(..), fromString)
import           Craft.Group (GroupID)
import           Craft.User (UserID)

import Control.Lens
import           Text.Megaparsec
import           Text.Megaparsec.String


setOwnerID :: UserID -> FilePath -> Craft ()
setOwnerID uid path = exec_ "/bin/chown" [show uid, path]


setGroupID :: GroupID -> FilePath -> Craft ()
setGroupID gid path = exec_ "/bin/chgrp" [show gid, path]


stat :: Args -> Craft ExecResult
stat = exec "stat"

getMode :: FilePath -> Craft Mode
getMode fp = do
  r <- stat ["-c", "%a", fp]
  return $ parseExecResult r modeParser (r ^. errorOnFail . stdout)


getOwnerID :: FilePath -> Craft UserID
getOwnerID fp = do
  r <- stat ["-c", "%u", fp]
  return $ parseExecResult r digitParser (r ^. errorOnFail . stdout)


getGroupID :: FilePath -> Craft GroupID
getGroupID fp = do
  r <- stat ["-c", "%g", fp]
  return $ parseExecResult r digitParser (r ^. errorOnFail . stdout)


getStats :: FilePath -> Craft (Maybe (Mode, UserID, GroupID))
getStats fp = stat ["-c", "%a:%u:%g", fp] >>= \case
  ExecFail _ -> return Nothing
  ExecSucc r ->
    return . Just $ parseExecResult (ExecSucc r) statsParser $ r ^. stdout


statsParser :: Parser (Mode, UserID, GroupID)
statsParser = do
  mode <- modeParser
  void $ char ':'
  owner <- digitParser
  void $ char ':'
  group <- digitParser
  return (mode, owner, group)


modeParser :: Parser Mode
modeParser = fromString <$> some digitChar


digitParser :: Parser Int
digitParser = read <$> some digitChar
