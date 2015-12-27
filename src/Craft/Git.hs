module Craft.Git where

import           Control.Lens hiding (noneOf)
import           Data.Maybe (isJust, fromMaybe)
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
    { url       :: URL
    , directory :: Directory.Path
    , version   :: Version
    , depth     :: Maybe Int
    }
  deriving (Eq, Show)


repo :: URL -> Directory.Path -> Repo
repo url directory =
  Repo
  { url       = url
  , directory = directory
  , version   = master
  , depth     = Nothing
  }


gitBin :: FilePath
gitBin = "/usr/bin/git"


git :: String -> [String] -> Craft ()
git cmd args = exec_ gitBin $ cmd : args


remotes :: Craft [String]
remotes = do
  r <- $errorOnFail =<< exec gitBin ["remote"]
  return $ r ^. stdout . to lines


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
  success <- $errorOnFail r
  results <- parseExecResult r repoURLParser $ success ^. stdout
  case lookup (origin, "fetch") results of
    Nothing -> $craftError $ "git remote `" ++ origin ++ "` not found."
    Just x  -> return x


-- TESTME
repoURLParser :: Parser [((String, String), String)]
repoURLParser = some $ do
  remote <- word
  url <- word
  direction <- char '(' *> some alphaNumChar <* char ')' <* newline
  return ((remote, direction), url)
 where
  word :: Parser String
  word = some (noneOf " \t") <* some (spaceChar <|> tab)


getVersion :: Craft Version
getVersion = do
  r <- exec gitBin ["rev-parse", "HEAD"]
  success <- $errorOnFail r
  Commit <$> parseExecResult r (some alphaNumChar) (success ^. stdout)


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
                    , depth     = Nothing
                    }


instance Craftable Repo where
  watchCraft r = do
    let dp = directory r
        ver = version r
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
          setURL $ url r
          git "fetch" [origin]
          git "checkout" ["--force", show ver]
          git "reset" ["--hard"]
          ver' <- getVersion
          verify ver'
          return ver'
    Directory.get dp >>= \case
      Nothing -> do
        case depth r of
          Nothing -> git "clone" [url r, dp]
          Just d  -> git "clone" ["--depth", show d, url r, dp]
        Directory.get dp >>= \case
          Nothing ->
            $craftError $ "craft Git.Repo `" ++ show r ++ "` failed! "
                       ++ "Clone Failed. Directory `" ++ dp ++ "` not found."
          Just dir ->
            withCWD dir $ do
              ver' <- checkoutCommit
              return (Created, r { version = ver' })

      Just dir ->
        withCWD dir $ do
          !beforeVersion <- getVersion
          ver' <- checkoutCommit
          case ver of
            Latest branchName -> git "pull" [origin, branchName]
            _                 -> return ()
          return ( if beforeVersion == ver' then Unchanged
                                            else Updated
                , r { version = ver' })
