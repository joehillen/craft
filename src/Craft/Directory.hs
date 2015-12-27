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
  { _path    :: Path
  , _mode    :: Mode
  , _ownerID :: UserID
  , _groupID :: GroupID
  }
  deriving (Eq, Show)
makeLenses ''Directory


owner :: Directory -> Craft User
owner d =
  User.fromID (d ^. ownerID) >>= \case
    Nothing -> $craftError
               $ formatToString ("No such owner with id `"%shown%"` for: "%shown)
                                (d ^. ownerID) d
    Just g  -> return g


group :: Directory -> Craft Group
group d =
  Group.fromID (d ^. groupID) >>= \case
    Nothing -> $craftError
               $ formatToString ("No such group with id `"%shown%"` for: "%shown)
                                (d ^. groupID) d
    Just g -> return g


directory :: Path -> Directory
directory dp =
  Directory
  { _path    = dp
  , _mode    = Mode RWX RX RX
  , _ownerID = 0
  , _groupID = 0
  }


multiple :: [Path] -> Mode -> User -> Group -> [Directory]
multiple paths mode owner' group' = map go paths
 where
  go path = Directory path mode (User.uid owner') (Group.gid group')


multipleRootOwned :: [Path] -> Mode -> [Directory]
multipleRootOwned paths m = map go paths
 where
  go path = directory path & mode .~ m


exists :: Path -> Craft Bool
exists p = isExecSucc <$> exec "/usr/bin/test" ["-d", p]


instance Craftable Directory where
  watchCraft d = do
    let dp = d ^. path
        setMode'  = setMode (d ^. mode) dp
        setOwner' = setOwnerID (d ^. ownerID) dp
        setGroup' = setGroupID (d ^. groupID) dp
        error' str = $craftError
           $ formatToString ("craft Directory `"%string%"` failed! "%string)
                            dp str
        verifyMode m =
          when (m /= d ^. mode) $
            error' $ formatToString ("Wrong Mode: "%shown%" Expected: "%shown)
                                    m (d ^. mode)
        verifyOwner o =
          when (o /= d ^. ownerID) $
            error' $ formatToString ("Wrong Owner ID: "%shown%" Expected: "%shown)
                                    o (d ^. ownerID)
        verifyGroup g =
          when (g /= d ^. groupID) $
            error' $ formatToString ("Wrong Group ID: "%shown%" Expected: "%shown)
                                    g (d ^. groupID)
        verifyStats (m, o, g) =
          verifyMode m >> verifyOwner o >> verifyGroup g
    getStats dp >>= \case
      Nothing -> do
        exec_ "mkdir" ["-p", dp]
        setMode' >> setOwner' >> setGroup'
        getStats dp >>= \case
          Nothing -> error' "Not Found."
          Just stats' -> verifyStats stats' >> return (Created, d)
      Just (m', o', g') -> do
        let checks = [ (d^.mode    == m', setMode')
                     , (d^.ownerID == o', setOwner')
                     , (d^.groupID == g', setGroup')
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
  success <- $errorOnFail r
  fns <- parseExecResult r getFilesParser $ success ^. stdout
  catMaybes <$> mapM (File.get . (</> dp)) fns
