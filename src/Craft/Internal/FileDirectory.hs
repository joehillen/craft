module Craft.Internal.FileDirectory
( module Craft.Internal.FileDirectory
, FilePath
) where

import           Craft.Internal
import           Craft.File.Mode (Mode(..), fromText)
import           Craft.Group (GroupID)
import           Craft.User (UserID)

import Control.Lens
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Text


setOwnerID :: UserID -> FilePath -> Craft ()
setOwnerID uid path = exec_ "/bin/chown" [T.pack (show uid), T.pack path]


setGroupID :: GroupID -> FilePath -> Craft ()
setGroupID gid path = exec_ "/bin/chgrp" [T.pack (show gid), T.pack path]


stat :: Args -> Craft ExecResult
stat = exec "stat"

getMode :: FilePath -> Craft Mode
getMode fp = do
  r <- stat ["-c", "%a", T.pack fp]
  success <- $errorOnFail r
  parseExecResult r modeParser (success ^. stdout)


getOwnerID :: FilePath -> Craft UserID
getOwnerID fp = do
  r <- stat ["-c", "%u", T.pack fp]
  success <- $errorOnFail r
  parseExecResult r digitParser (success ^. stdout)


getGroupID :: FilePath -> Craft GroupID
getGroupID fp = do
  r <- stat ["-c", "%g", T.pack fp]
  success <- $errorOnFail r
  parseExecResult r digitParser (success ^. stdout)


getStats :: FilePath -> Craft (Maybe (Mode, UserID, GroupID))
getStats fp =
  stat ["-c", "%a:%u:%g", T.pack fp] >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> Just <$> parseExecResult (ExecSucc r) statsParser (r ^. stdout)


statsParser :: Parser (Mode, UserID, GroupID)
statsParser = do
  mode <- modeParser
  void $ char ':'
  owner <- digitParser
  void $ char ':'
  group <- digitParser
  return (mode, owner, group)


modeParser :: Parser Mode
modeParser = fromText . T.pack <$> some digitChar


digitParser :: Parser Int
digitParser = read <$> some digitChar
