module Craft.Directory
( module Craft.Directory
, setGroupID
, setOwnerID
, getOwnerID
, getGroupID
, getMode
)
where

import           Craft.Internal
import           Craft.File (File)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.Group (Group, GroupID)
import qualified Craft.Group as Group
import           Craft.Internal.FileDirectory
import           Craft.User (User, UserID)
import qualified Craft.User as User

import           Data.List (intercalate)
import           Data.Maybe (catMaybes, isJust)
import           Text.Megaparsec
import           System.FilePath

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
    Nothing -> $craftError $ "No such owner with id `" ++ show (ownerID d)
                             ++ "` for: " ++ show d
    Just g  -> return g


group :: Directory -> Craft Group
group d =
  Group.fromID (groupID d) >>= \case
    Nothing -> $craftError $ "No such group with id `" ++ show (groupID d)
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
  watchCraft d = do
    let dp = path d
        setMode'  = setMode (mode d) dp
        setOwner' = setOwnerID (ownerID d) dp
        setGroup' = setGroupID (groupID d) dp
        error' str = error $ "craft Directory `" ++ dp ++ "` failed! " ++ str
        verifyMode m =
          when (m /= mode d) $ error' $ "Wrong Mode: " ++ show m
                                    ++ " Expected: " ++ show (mode d)
        verifyOwner o =
          when (o /= ownerID d) $ error' $ "Wrong Owner ID: " ++ show o
                                       ++ " Expected: " ++ show (ownerID d)
        verifyGroup g =
          when (g /= groupID d) $ error' $ "Wrong Group ID: " ++ show g
                                       ++ " Expected: " ++ show (groupID d)
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
        if and (map fst checks) then
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
  r <- exec "/bin/ls" ["-a", "-1", dp]
  let fns = parseExecResult r getFilesParser $ stdout $ errorOnFail r
  catMaybes <$> mapM (File.get . (</> dp)) fns


