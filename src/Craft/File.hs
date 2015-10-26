module Craft.File
( module Craft.File
, setGroupID
, setOwnerID
, getOwnerID
, getGroupID
, getMode
)
where

import           Craft
import           Craft.File.Mode
import           Craft.User (User, UserID)
import qualified Craft.User as User
import           Craft.Group (Group, GroupID)
import qualified Craft.Group as Group
import           Craft.Internal.FileDirectory

import           Control.Monad (unless)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe (fromJust, isJust)


type Path = FilePath


data File
  = File
    { path    :: Path
    , mode    :: Mode
    , ownerID :: UserID
    , groupID :: GroupID
    , content :: Maybe ByteString
    }
   deriving (Eq)


owner :: File -> Craft User
owner f =
  User.fromID (ownerID f) >>= \case
    Nothing -> error $ "No such owner with id `" ++ show (ownerID f)
                       ++ "` for: " ++ show f
    Just g  -> return g


group :: File -> Craft Group
group f =
  Group.fromID (groupID f) >>= \case
    Nothing -> error $ "No such group with id `" ++ show (groupID f)
                       ++ "` for: " ++ show f
    Just g -> return g


instance Show File where
  show f = "File { path = " ++ show (path f) ++
                ", mode = " ++ show (mode f) ++
                ", owner = " ++ show (ownerID f) ++
                ", group = " ++ show (groupID f) ++
                ", content = " ++ showContent (content f) ++
               " }"
    where
      showContent Nothing  = "Nothing"
      showContent (Just c) = "Just " ++ show (BS.take 30 c) ++ "..."


name :: File -> String
name f = takeFileName $ path f


strContent :: String -> Maybe ByteString
strContent = Just . B8.pack


contentAsString :: File -> String
contentAsString = B8.unpack . fromJust . content


file :: Path -> File
file fp =
  File
  { path    = fp
  , mode    = Mode RW R R
  , ownerID = 0
  , groupID = 0
  , content = Nothing
  }


multiple :: [Path] -> Mode -> User -> Group -> Maybe ByteString -> [File]
multiple paths mode owner group content = map go paths
 where
  go path = File path mode (User.uid owner) (Group.gid group) content


multipleRootOwned :: [Path] -> Mode -> Maybe ByteString -> [File]
multipleRootOwned paths mode content = map go paths
 where
  go path = (file path) { mode = mode
                        , content = content
                        }


instance Craftable File where
  checker = get . path
  destroyer = notImplemented "destroyer File"
  crafter f mf = do
    unless (isJust mf) $ exec_ "touch" [path f]

    let fp = path f
    let setMode' = setMode (mode f) fp
    let setOwner' = setOwnerID (ownerID f) $ path f
    let setGroup' = setGroupID (groupID f) $ path f

    case mf of
      Nothing -> do
        setMode'
        setOwner'
        setGroup'
      Just oldf -> do
        unless (mode f == mode oldf) $ setMode'
        unless (ownerID f == ownerID oldf) $ setOwner'
        unless (groupID f == groupID oldf) $ setGroup'

    case content f of
      Nothing -> return ()
      Just c -> write (path f) c


write :: Path -> ByteString -> Craft ()
write = fileWrite


exists :: Path -> Craft Bool
exists fp = isExecSucc <$> exec "/usr/bin/test" ["-f", fp]


get :: Path -> Craft (Maybe File)
get fp = do
  exists' <- exists fp
  if not exists' then
    return Nothing
  else do
    m <- getMode fp
    o <- getOwnerID fp
    g <- getGroupID fp
    content <- fileRead fp
    return . Just $
      File { path    = fp
           , mode    = m
           , ownerID = o
           , groupID = g
           , content = Just content
           }


md5sum :: Path -> Craft String
md5sum fp = head . words . stdout . errorOnFail <$> exec "/usr/bin/md5sum" [fp]
