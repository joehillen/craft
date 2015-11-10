module Craft.Git where

import           Control.Monad (unless)
import           Data.Maybe (isJust, fromMaybe)
import           Text.Megaparsec

import           Craft hiding (Version(..))
import qualified Craft.Directory as Directory


type BranchName = String
type TagName    = String
type SHA        = String


data Version
  = Branch BranchName
  | Latest BranchName
  | Tag    TagName
  | Commit SHA
 deriving (Eq)


instance Show Version where
  show (Branch name) = name
  show (Latest name) = name
  show (Tag    name) = "tags/" ++ name
  show (Commit sha)  = sha


master :: Version
master = Latest "master"


origin :: String
origin = "origin"


type URL = String


data Repo
  = Repo
    { url       :: URL
    , directory :: Directory.Path
    , version   :: Version
    }
  deriving (Eq, Show)


repo :: URL -> Directory.Path -> Repo
repo url directory =
  Repo
  { url       = url
  , directory = directory
  , version   = master
  }


gitBin :: FilePath
gitBin = "/usr/bin/git"


git :: String -> [String] -> Craft ()
git cmd args = exec_ gitBin $ cmd : args


remotes :: Craft [String]
remotes = lines . stdout . errorOnFail <$> exec gitBin ["remote"]


setURL :: URL -> Craft ()
setURL url = do
  rs <- remotes
  if origin `elem` rs then
    git "remote" ["set-url", origin, url]
  else
    git "remote" ["add", origin, url]


getURL :: Craft URL
getURL = do
  r <- exec gitBin ["remote", "-v"]
  let results = parseExecResult r repoURLParser $ stdout $ errorOnFail r
  return $ fromMaybe (error $ "git remote `" ++ origin ++ "` not found.")
                     (lookup (origin, "fetch") results)


-- TESTME
repoURLParser :: Parsec String [((String, String), String)]
repoURLParser = some $ do
  remote <- word
  url <- word
  direction <- char '(' *> some alphaNumChar <* char ')' <* newline
  return ((remote, direction), url)
 where
  word = some (noneOf " \t") <* some (spaceChar <|> tab)


getVersion :: Craft Version
getVersion = do
  r <- exec gitBin ["rev-parse", "HEAD"]
  return . Commit $ parseExecResult r parser $ stdout $ errorOnFail r
 where
  parser = some alphaNumChar


get :: Directory.Path -> Craft (Maybe Repo)
get path =
  Directory.get path >>= \case
    Nothing -> return Nothing
    Just dir -> withCWD dir $ do
      !url'     <- getURL
      !version' <- getVersion
      return . Just
             $ Repo { directory = path
                    , url       = url'
                    , version   = version'
                    }


instance Craftable Repo where
  checker = get . directory

  crafter Repo{..} mrepo = do
    unless (isJust mrepo) $
      git "clone" [url, directory]

    dir <- fromMaybe (error $ "git clone failed! "
                              ++ "'" ++ directory ++ "' not found")
           <$> Directory.get directory
    withCWD dir $ do
      setURL url
      git "fetch" [origin]
      git "checkout" ["--force", show version]
      git "reset" ["--hard"]
      case version of
        Latest branchName -> git "pull" [origin, branchName]
        _                 -> return ()

  destroyer = notImplemented "destroyer Git"
