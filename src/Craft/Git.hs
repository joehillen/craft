module Craft.Git where

import           Control.Lens         hiding (noneOf)
import           Control.Monad.Trans.Maybe
import           Data.Void            (Void)
import           Formatting           hiding (char)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Craft
import qualified Craft.Directory      as Dir


type BranchName = String
type TagName    = String
type SHA        = String


data Rev
  = Branch BranchName
  | Pull   BranchName
  | Tag    TagName
  | Commit SHA
 deriving (Eq)


instance Show Rev where
  show (Branch name) = name
  show (Pull name) = name
  show (Tag    name) = "tags/" ++ name
  show (Commit sha)  = sha


origin :: String
origin = "origin"


type URL = String


data Repo
  = Repo
    { _gitUrl       :: URL
    , _gitDirectory :: Directory
    , _gitRev       :: Rev
    , _gitDepth     :: Maybe Int
    }
  deriving (Eq, Show)
makeLenses ''Repo

rev :: Lens' Repo Rev
rev = gitRev

repo :: URL -> AbsDirPath -> Repo
repo url' dirpath =
  Repo
  { _gitUrl       = url'
  , _gitDirectory = directory dirpath
  , _gitRev       = Pull "master"
  , _gitDepth     = Nothing
  }

instance FileLike Repo where
  type FileLikePath Repo = AbsDirPath
  path = gitDirectory.path
  mode = gitDirectory.directoryMode
  ownerID = gitDirectory.directoryOwnerID
  groupID = gitDirectory.directoryGroupID


gitBin :: String
gitBin = "git"


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
repoURLParser :: Parsec Void String [((String, String), String)]
repoURLParser = some $ do
  remote <- word
  url' <- word
  direction <- char '(' *> some alphaNumChar <* char ')' <* newline
  return ((remote, direction), url')
 where
  word :: Parsec Void String String
  word = some (noneOf [' ', '\t']) <* some (spaceChar <|> tab)


getRev :: Craft Rev
getRev = Commit <$> parseExecStdout (some alphaNumChar) gitBin ["rev-parse", "HEAD"]


get :: AbsDirPath -> Craft (Maybe Repo)
get dp = runMaybeT $ do
  dir <- MaybeT $ Dir.get dp
  MaybeT $ inDirectory dir $
    Just <$> (Repo <$> getURL
                   <*> pure dir
                   <*> getRev
                   <*> pure Nothing)


instance Craftable Repo Repo where
  watchCraft desiredRepo = do
    let dp = desiredRepo ^. gitDirectory . path
    let desiredRev = desiredRepo ^. gitRev
    let verify actualRev =
          case desiredRev of
            Commit _ ->
              when (actualRev /= desiredRev) $
                $craftError
                $ formatToString ("craft Git.DesiredRepo `"%shown%"` failed! Wrong Commit: "%shown%" Got: "%shown)
                                 desiredRepo desiredRev actualRev
            _ -> return ()

    let checkoutCommit :: Craft Rev
        checkoutCommit = do
          setURL $ desiredRepo ^. gitUrl
          git "fetch" [origin]
          git "checkout" ["--force", show desiredRev]
          git "reset" ["--hard"]
          actualRev <- getRev
          verify actualRev
          return actualRev

    let depthArg = case desiredRepo ^. gitDepth of
          Nothing -> []
          Just d  -> ["--depth", show d]

    res <-
      Dir.get dp >>= \case
        Nothing -> do
          git "clone" $ depthArg++[desiredRepo ^. gitUrl, fromAbsDir dp]
          Dir.get dp >>= \case
            Nothing -> $craftError $ "craft Git.Repo `"++show desiredRepo++"` failed! "
                                  ++ "Clone Failed. Directory not found: "++show dp
            Just dir -> do
              craft_ $ desiredRepo ^. gitDirectory
              inDirectory dir $ do
                actualRev <- checkoutCommit
                return (Created, desiredRepo & gitRev .~ actualRev)

        Just dir -> do
          craft_ $ desiredRepo ^. gitDirectory
          inDirectory dir $ do
            !beforeRev <- getRev
            actualRev <- checkoutCommit
            case desiredRev of
              Pull branchName -> git "pull" [origin, branchName]
              _               -> return ()
            let w = if beforeRev == actualRev
                      then Unchanged
                      else Updated
            return (w, desiredRepo & gitRev .~ actualRev)
    when (changed (fst res)) $
      exec_ "chown" [((show $ desiredRepo^.ownerID)++":"++(show $ desiredRepo^.groupID)), "-R", fromAbsDir (desiredRepo^.path)]
    return res
