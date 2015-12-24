module Craft.Directory
( module Craft.Directory
, setGroupID
, setOwnerID
, getOwnerID
, getGroupID
, getMode
)
where

import Control.Lens
import           Craft.Internal
import           Craft.File (File)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.Group (Group, GroupID)
import qualified Craft.Group as Group
import           Craft.Internal.FileDirectory
import           Craft.User (User, UserID)
import qualified Craft.User as User
import Craft.Directory.Parser

import           Data.Maybe (catMaybes)
import           System.FilePath
import           Formatting

type Path = FilePath


data Directory =
  Directory
  { path    :: Path
  , mode    :: Mode
  , ownerID :: UserID
  , groupID :: GroupID
  }
  deriving (Eq, Show)


owner :: Directory -> Craft User
owner d =
  User.fromID (ownerID d) >>= \case
    Nothing -> $craftError
               $ formatToString ("No such owner with id `"%shown%"` for: "%shown)
                                (ownerID d) d
    Just g  -> return g


group :: Directory -> Craft Group
group d =
  Group.fromID (groupID d) >>= \case
    Nothing -> $craftError
               $ formatToString ("No such group with id `"%shown%"` for: "%shown)
                                (groupID d) d
    Just g -> return g


directory :: Path -> Directory
directory dp =
  Directory
  { path    = dp
  , mode    = Mode RWX RX RX
  , ownerID = 0
  , groupID = 0
  }


multiple :: [Path] -> Mode -> User -> Group -> [Directory]
multiple paths mode owner' group' = map go paths
 where
  go path = Directory path mode (User.uid owner') (Group.gid group')


multipleRootOwned :: [Path] -> Mode -> [Directory]
multipleRootOwned paths mode = map go paths
 where
  go path = (directory path) { mode = mode }


exists :: Path -> Craft Bool
exists p = isExecSucc <$> exec "/usr/bin/test" ["-d", p]


instance Craftable Directory where
  watchCraft d = do
    let dp = path d
        setMode'  = setMode (mode d) dp
        setOwner' = setOwnerID (ownerID d) dp
        setGroup' = setGroupID (groupID d) dp
        error' str = $craftError
           $ formatToString ("craft Directory `"%string%"` failed! "%string)
                            dp str
        verifyMode m =
          when (m /= mode d) $
            error' $ formatToString ("Wrong Mode: "%shown%" Expected: "%shown)
                                    m (mode d)
        verifyOwner o =
          when (o /= ownerID d) $
            error' $ formatToString ("Wrong Owner ID: "%shown%" Expected: "%shown)
                                    o (ownerID d)
        verifyGroup g =
          when (g /= groupID d) $
            error' $ formatToString ("Wrong Group ID: "%shown%" Expected: "%shown)
                                    g (groupID d)
        verifyStats (m, o, g) =
          verifyMode m >> verifyOwner o >> verifyGroup g
    getStats dp >>= \case
      Nothing -> do
        exec_ "mkdir" ["-p", path d]
        setMode' >> setOwner' >> setGroup'
        getStats dp >>= \case
          Nothing -> error' "Not Found."
          Just stats' -> verifyStats stats' >> return (Created, d)
      Just (m', o', g') -> do
        let checks = [ (mode d == m', setMode')
                     , (ownerID d == o', setOwner')
                     , (groupID d == g', setGroup')
                     ]
        mapM_ (uncurry unless) checks
        if all fst checks then
          return (Unchanged, d)
        else
          getStats dp >>= \case
            Nothing -> error' "Not Found."
            Just stats' -> verifyStats stats' >> return (Updated, d)


get :: Path -> Craft (Maybe Directory)
get dp =
  getStats dp >>= \case
    Nothing -> return Nothing
    Just (m, o, g) -> return . Just $ Directory dp m o g


getFiles :: Path -> Craft [File]
getFiles dp = do
  r <- exec "/bin/ls" ["-a", "-1", dp]
  let fns = parseExecResult r getFilesParser $ r ^. errorOnFail . stdout
  catMaybes <$> mapM (File.get . (</> dp)) fns
