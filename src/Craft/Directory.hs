module Craft.Directory
( module Craft.Directory
, setGroup
, setOwner
, getOwner
, getGroup
, getMode
)
where

import           Craft
import           Craft.File (File)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.Group (Group)
import qualified Craft.Group as Group
import           Craft.Internal.FileDirectory
import           Craft.Internal.Helpers
import           Craft.User (User)
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
  { path  :: Path
  , mode  :: Mode
  , owner :: Maybe User
  , group :: Maybe Group
  }
  deriving (Eq, Show)


directory :: Path -> Directory
directory dp =
  Directory
  { path  = dp
  , mode  = Mode RWX RX RX
  , owner = Nothing
  , group = Nothing
  }


multiple :: [Path] -> Mode -> User -> Group -> [Directory]
multiple paths mode owner group = map go paths
 where
  go path = Directory path mode (Just owner) (Just group)


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
    let setOwner' = whenJust (owner d) $ setOwner $ path d
    let setGroup' = whenJust (group d) $ setGroup $ path d
    case md of
      Nothing -> do
        setMode'
        setOwner'
        setGroup'
      Just oldd -> do
        unless (mode d == mode oldd) $ setMode'
        unless ((User.name <$> (owner d))
                == (User.name <$> (owner oldd))) $ setOwner'
        unless ((Group.name <$> (group d))
                == (Group.name <$> (group oldd))) $ setGroup'

  destroyer = notImplemented "destroyer Directory"


get :: Path -> Craft (Maybe Directory)
get dp = do
  exists' <- exists dp
  if not exists' then
      return Nothing
  else do
    m <- getMode dp
    o <- getOwner dp
    g <- getGroup dp
    return . Just $
      Directory { path  = dp
                , mode  = m
                , owner = Just o
                , group = Just g
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
