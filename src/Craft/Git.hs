module Craft.Git where

import           Control.Lens hiding (noneOf)
import           Text.Megaparsec
import           Text.Megaparsec.String
import Formatting hiding (char)

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
    { _url       :: URL
    , _directory :: Directory.Path
    , _version   :: Version
    , _depth     :: Maybe Int
    }
  deriving (Eq, Show)
makeLenses ''Repo


repo :: URL -> Directory.Path -> Repo
repo url' directory' =
  Repo
  { _url       = url'
  , _directory = directory'
  , _version   = master
  , _depth     = Nothing
  }


gitBin :: FilePath
gitBin = "/usr/bin/git"


git :: String -> [String] -> Craft ()
git cmd args = exec_ gitBin $ cmd : args


remotes :: Craft [String]
remotes = lines <$> ($stdoutOrError =<< exec gitBin ["remote"])


setURL :: URL -> Craft ()
setURL url' = do
  rs <- remotes
  if origin `elem` rs then
    git "remote" ["set-url", origin, url']
  else
    git "remote" ["add", origin, url']


getURL :: Craft URL
getURL = do
  results <- parseExecStdout repoURLParser gitBin ["remote", "-v"]
  case lookup (origin, "fetch") results of
    Nothing -> $craftError $ "git remote `" ++ origin ++ "` not found."
    Just x  -> return x


-- TESTME
repoURLParser :: Parser [((String, String), String)]
repoURLParser = some $ do
  remote <- word
  url' <- word
  direction <- char '(' *> some alphaNumChar <* char ')' <* newline
  return ((remote, direction), url')
 where
  word :: Parser String
  word = some (noneOf " \t") <* some (spaceChar <|> tab)


getVersion :: Craft Version
getVersion = Commit <$> parseExecStdout (some alphaNumChar) gitBin ["rev-parse", "HEAD"]


get :: Directory.Path -> Craft (Maybe Repo)
get path =
  Directory.get path >>= \case
    Nothing -> return Nothing
    Just dir -> withCWD dir $ do
      !url'     <- getURL
      !version' <- getVersion
      return . Just
             $ Repo { _directory = path
                    , _url       = url'
                    , _version   = version'
                    , _depth     = Nothing
                    }


instance Craftable Repo where
  watchCraft r = do
    let dp = r ^. directory
        ver = r ^. version
        verify ver' =
          case ver of
            Commit _ ->
              when (ver' /= ver) $
                $craftError
                $ formatToString ("craft Git.Repo `"%shown%"` failed! Wrong Commit: "%shown%" Got: "%shown)
                                 r ver ver'
            _ -> return ()
        checkoutCommit :: Craft Version
        checkoutCommit = do
          setURL $ r ^. url
          git "fetch" [origin]
          git "checkout" ["--force", show ver]
          git "reset" ["--hard"]
          ver' <- getVersion
          verify ver'
          return ver'

    Directory.get dp >>= \case
      Nothing -> do
        case r ^. depth of
          Nothing -> git "clone" [r ^. url, dp]
          Just d  -> git "clone" ["--depth", show d, r ^. url, dp]
        Directory.get dp >>= \case
          Nothing ->
            $craftError $ "craft Git.Repo `" ++ show r ++ "` failed! "
                       ++ "Clone Failed. Directory `" ++ dp ++ "` not found."
          Just dir ->
            withCWD dir $ do
              ver' <- checkoutCommit
              return (Created, r & version .~ ver' )

      Just dir ->
        withCWD dir $ do
          !beforeVersion <- getVersion
          ver' <- checkoutCommit
          case ver of
            Latest branchName -> git "pull" [origin, branchName]
            _                 -> return ()
          return ( if beforeVersion == ver' then Unchanged else Updated
                 , r & version .~ ver')
