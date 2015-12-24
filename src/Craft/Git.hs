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
remotes = view (errorOnFail . stdout . to lines) <$> exec gitBin ["remote"]


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
  let results = parseExecResult r repoURLParser $ r ^. errorOnFail . stdout
  return $ fromMaybe (error $ "git remote `" ++ origin ++ "` not found.")
                     (lookup (origin, "fetch") results)


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
  return . Commit $ parseExecResult r parser $ r ^. errorOnFail . stdout
 where
  parser :: Parser String
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
                    , depth     = Nothing
                    }


instance Craftable Repo where
  watchCraft r = do
    let dp = directory r
        ver = version r
        verify ver' = do
          case ver of
            Commit _ ->
              when (ver' /= ver) $
                $craftError
                $ formatToString ("craft Git.Repo `"%shown%"` failed! Wrong Commit: "%shown%" Got: "%shown)
                                 r ver ver'

            _ -> return ()
    Directory.get dp >>= \case
      Nothing -> do
        case depth r of
          Nothing -> git "clone" [url r, dp]
          Just d  -> git "clone" ["--depth", show d, url r, dp]
        Directory.get dp >>= \case
          Nothing -> error $ "craft Git.Repo `" ++ show r ++ "` failed! "
                          ++ "Clone Failed. Directory `" ++ dp ++ "` not found."
          Just dir ->
            withCWD dir $ do
              setURL (url r)
              git "fetch" [origin]
              git "checkout" ["--force", show ver]
              git "reset" ["--hard"]
              ver' <- getVersion
              verify ver'
              return (Created, r { version = ver' })

      Just dir ->
        withCWD dir $ do
          !beforeVersion <- getVersion
          setURL (url r)
          git "fetch" [origin]
          git "checkout" ["--force", show ver]
          git "reset" ["--hard"]
          case ver of
            Latest branchName -> git "pull" [origin, branchName]
            _                 -> return ()
          !ver' <- getVersion
          verify ver'
          return ( if beforeVersion == ver' then Unchanged
                                            else Updated
                , r { version = ver' })
