module Craft.Directory
( module Craft.Directory
, setGroupID
, setOwnerID
, getOwnerID
, getGroupID
, getMode
)
where

import           Control.Lens
import           Data.Maybe             (catMaybes)
import           Formatting

import           Craft.Directory.Parser
import qualified Craft.File             as File
import           Craft.File.Mode
import qualified Craft.Group            as Group
import           Craft.Internal
import qualified Craft.User             as User


name :: Lens' Directory RelDirPath
name = dirName


getOwner :: Directory -> Craft User
getOwner d =
  User.fromID (d ^. ownerID) >>= \case
    Nothing -> $craftError $ formatToString ("No such owner with `"%shown%"` for: "%shown)
                                            (d ^. ownerID) d
    Just g  -> return g


getGroup :: Directory -> Craft Group
getGroup d =
  Group.fromID (d ^. groupID) >>= \case
    Nothing -> $craftError $ formatToString ("No such group with `"%shown%"` for: "%shown)
                                            (d ^. groupID) d
    Just g -> return g


multiple :: [AbsDirPath] -> Mode -> User -> Group -> [Directory]
multiple paths mode' owner' group' = map go paths
 where
  go p = Directory p mode' (owner'^.uid) (group'^.gid)


multipleRootOwned :: [AbsDirPath] -> Mode -> [Directory]
multipleRootOwned paths m = map go paths
 where
  go p = directory p & mode .~ m


exists :: AbsDirPath -> Craft Bool
exists p = isSuccess <$> exec "test" ["-d", fromAbsDir p]


get :: AbsDirPath -> Craft (Maybe Directory)
get dp = do
  s <- getStats dp
  return $ do
    (m, o, g) <- s
    return $ Directory dp m o g


getFiles :: AbsDirPath -> Craft [File]
getFiles dp = do
  fns <- mapM parseRelFile =<< parseExecStdout getFilesParser "ls" ["-a", "-1", fromAbsDir dp]
  catMaybes <$> mapM (File.get . (dp </>)) fns


-- | A thin wrapper over the Unix find program.
find :: AbsDirPath -> Args -> Craft [Directory]
find dir args = do
  ds <- mapM parseAbsDir . drop 1 . lines =<< $stdoutOrError =<< exec "find" ([fromAbsDir dir] ++ args ++ ["-type", "d"])
  catMaybes <$> (mapM get ds)
