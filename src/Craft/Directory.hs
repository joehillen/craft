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
import           Craft.User (User)
import qualified Craft.User as User

import           Control.Monad.Extra (unlessM)
import           Data.List (intercalate)
import           Data.Maybe (catMaybes)
import           Text.Parsec
import           Text.Parsec.String (Parser)

type Path = FilePath

data Directory =
  Directory
  { path  :: Path
  , mode  :: Mode
  , owner :: User
  , group :: Group
  }
  deriving (Eq, Show)

directory :: Path -> Directory
directory dp =
  Directory
  { path  = dp
  , mode  = Mode RWX RX RX
  , owner = User.root
  , group = Group.root
  }

multiple :: [Path] -> Mode -> User -> Group -> [Directory]
multiple paths mode owner group = map go paths
 where
  go path = Directory path mode owner group

multipleRootOwned :: [Path] -> Mode -> [Directory]
multipleRootOwned paths mode = map go paths
 where
  go path = (directory path) { mode = mode }

exists :: Path -> Craft Bool
exists p = isSuccess . exitcode <$> exec "/usr/bin/test" ["-d", p]

instance Craftable Directory where
  checker = get . path
  crafter Directory{..} = do
    unlessM (exists path) $
      exec_ "mkdir" ["-p", path]
    setMode mode path
    setOwner owner path
    setGroup group path
  remover = notImplemented "remover Directory"

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
                , owner = o
                , group = g
                }

getFilesParser :: Parser [String]
getFilesParser = stuff `sepEndBy` newline
 where
  stuff :: Parser String
  stuff = do
    optional $ string "." >> newline
    optional $ string ".." >> newline
    line
  line :: Parser String
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
  fns <- parseExec getFilesParser "/bin/ls" ["-a", "-1", dp]
  catMaybes <$> mapM (File.get . (</> dp)) fns
