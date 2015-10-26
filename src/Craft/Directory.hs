module Craft.Directory
( module Craft.Directory
, setGroupID
, setOwnerID
, getOwnerID
, getGroupID
, getMode
)
where

import           Craft
import           Craft.File (File)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.Group (Group, GroupID)
import qualified Craft.Group as Group
import           Craft.Internal.FileDirectory
import           Craft.Internal.Helpers
import           Craft.User (User, UserID)
import qualified Craft.User as User

import           Control.Monad (void, unless)
import           Control.Monad.Extra (unlessM)
import           Data.List (intercalate)
import           Data.Maybe (catMaybes)
import           Text.Megaparsec
import           Data.Maybe (isJust)


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
    Nothing -> error $ "No such owner with id `" ++ show (ownerID d)
                       ++ "` for: " ++ show d
    Just g  -> return g


group :: Directory -> Craft Group
group d =
  Group.fromID (groupID d) >>= \case
    Nothing -> error $ "No such group with id `" ++ show (groupID d)
                       ++ "` for: " ++ show d
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
  checker = get . path
  crafter d md = do
    unless (isJust md) $
      exec_ "mkdir" ["-p", path d]

    let setMode'  = setMode (mode d) $ path d
    let setOwner' = setOwnerID (ownerID d) $ path d
    let setGroup' = setGroupID (groupID d) $ path d
    case md of
      Nothing -> do
        setMode'
        setOwner'
        setGroup'
      Just oldd -> do
        unless (mode d == mode oldd) setMode'
        unless (ownerID d == ownerID oldd) setOwner'
        unless (groupID d == groupID oldd) setGroup'

  destroyer = notImplemented "destroyer Directory"


get :: Path -> Craft (Maybe Directory)
get dp = do
  exists' <- exists dp
  if not exists' then
      return Nothing
  else do
    m <- getMode dp
    o <- getOwnerID dp
    g <- getGroupID dp
    return . Just $
      Directory { path    = dp
                , mode    = m
                , ownerID = o
                , groupID = g
                }


getFilesParser :: Parsec String [String]
getFilesParser = stuff `sepBy` newline <* optional newline
 where
  stuff :: Parsec String String
  stuff = do
    void $ optional $ string "." >> newline
    void $ optional $ string ".." >> newline
    line
  line :: Parsec String String
  line = many $ noneOf "\n"


testGetFilesParser :: IO Bool
testGetFilesParser = do
  let expected = ["ab", "lkjasd", "912 12391", " ", "~"] :: [String]
  let resultE = parse getFilesParser "testGetFilesParser"
                $ intercalate "\n" (".":"..":expected)
  case resultE of
    Left err -> do
      putStrLn $ "FAILED: error " ++ show err
      return False
    Right result ->
      if result /= expected then do
        putStrLn $ "FAILED: got " ++ show expected
        return False
      else
        return True


getFiles :: Path -> Craft [File]
getFiles dp = do
  fns <- parseExec getFilesParser stdout "/bin/ls" ["-a", "-1", dp]
  catMaybes <$> mapM (File.get . (</> dp)) fns
