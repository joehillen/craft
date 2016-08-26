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
import           System.FilePath

import           Craft.Directory.Parser
import qualified Craft.File             as File
import           Craft.File.Mode
import qualified Craft.Group            as Group
import           Craft.Internal
import qualified Craft.User             as User


type Path = FilePath


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


multiple :: [Path] -> Mode -> User -> Group -> [Directory]
multiple paths mode' owner' group' = map go paths
 where
  go p = Directory p mode' (owner'^.uid) (group'^.gid)


multipleRootOwned :: [Path] -> Mode -> [Directory]
multipleRootOwned paths m = map go paths
 where
  go p = directory p & mode .~ m


exists :: Path -> Craft Bool
exists p = isExecSucc <$> exec "test" ["-d", p]


get :: Path -> Craft (Maybe Directory)
get dp =
  getStats dp >>= \case
    Nothing -> return Nothing
    Just (m, o, g) -> return . Just $ Directory dp m o g


getFiles :: Path -> Craft [File]
getFiles dp = do
  fns <- parseExecStdout getFilesParser "ls" ["-a", "-1", dp]
  catMaybes <$> mapM (File.get . (</> dp)) fns
