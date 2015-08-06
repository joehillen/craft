module Craft.File
( module Craft.File
, setGroup
, setOwner
, getOwner
, getGroup
, getMode
)
where

import           Craft
import           Craft.File.Mode
import           Craft.User (User)
import qualified Craft.User as User
import           Craft.Group (Group)
import qualified Craft.Group as Group
import           Craft.Internal.FileDirectory

import           Control.Monad.Extra (unlessM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe (fromJust)

type Path = FilePath

data File
  = File
    { path  :: Path
    , mode  :: Mode
    , owner :: User
    , group :: Group
    , content :: Maybe ByteString
    }
   deriving (Eq, Show)

name :: File -> String
name f = takeFileName $ path f

strContent :: String -> Maybe ByteString
strContent = Just . B8.pack

contentAsString :: File -> String
contentAsString = B8.unpack . fromJust . content

file :: Path -> File
file fp =
  File
  { path  = fp
  , mode  = Mode RW R R
  , owner = User.root
  , group = Group.root
  , content = Nothing
  }

multiple :: [Path] -> Mode -> User -> Group -> Maybe ByteString -> [File]
multiple paths mode owner group content = map go paths
 where
  go path = File path mode owner group content


multipleRootOwned :: [Path] -> Mode -> Maybe ByteString -> [File]
multipleRootOwned paths mode content = map go paths
 where
  go path = (file path) { mode = mode
                        , content = content
                        }

instance Craftable File where
  checker = get . path
  remover = notImplemented "File.remover"
  crafter File{..} = do
    unlessM (exists path) $
      write path ""

    setMode mode path
    setOwner owner path
    setGroup group path

    case content of
      Nothing -> return ()
      Just c -> write path c

write :: Path -> ByteString -> Craft ()
write fp c = do
  ex <- asks craftExecuter
  fileWriter ex fp c

exists :: Path -> Craft Bool
exists fp = do
  (code, _, _) <- exec "/usr/bin/test" ["-f", fp]
  return $ isSuccess code

get :: Path -> Craft (Maybe File)
get fp = do
  exists' <- exists fp
  if not exists' then
    return Nothing
  else do
    m <- getMode fp
    o <- getOwner fp
    g <- getGroup fp
    ex <- asks craftExecuter
    c <- fileReader ex fp
    return . Just $
      File { path    = fp
            , mode    = m
            , owner   = o
            , group   = g
            , content = Just c
            }

md5sum :: Path -> Craft String
md5sum fp = do
  (_, stdout, _) <- exec "/usr/bin/md5sum" [fp]
  return $ head $ words stdout
