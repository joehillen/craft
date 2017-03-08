module Craft.DSL where

import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.Reader
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as B8
import           Data.List              (intercalate)
import           Data.List.Split        (splitOn)
import qualified Data.Map.Strict        as Map
import           Text.Megaparsec
import           Text.Megaparsec.String

import           Craft.Types


-- | Craft DSL
exec :: Command -> Args -> Craft ExecResult
exec cmd args = do
  ce <- ask
  liftF $ Exec ce cmd args id


execAs :: User -> Command -> Args -> Craft ExecResult
execAs user cmd args = withUser user $ exec cmd args


exec_ :: Command -> Args -> Craft ()
exec_ cmd args = do
  ce <- ask
  liftF $ Exec_ ce cmd args ()


execAs_ :: User -> Command -> Args -> Craft ExecResult
execAs_ user cmd args = withUser user $ exec cmd args


fileRead :: AbsFilePath -> Craft BS.ByteString
fileRead fp =
  liftF $ FileRead fp id


fileWrite :: AbsFilePath -> BS.ByteString -> Craft ()
fileWrite fp content =
  liftF $ FileWrite fp content ()


sourceFile :: (IO FilePath) -> AbsFilePath -> Craft ()
sourceFile sourcer dest = liftF $ SourceFile sourcer dest ()


parseExecResult :: ExecResult -> Parser a -> String -> Craft a
parseExecResult execr parser str =
  case parse parser (showProcess $ execResultProcess execr) str of
    Right x -> return x
    Left err -> $craftError $
      unlines
      [ "parseExecResult error:"
      , "<<<< process >>>>"
      , showProcess $ execResultProcess execr
      , "<<<< output >>>>"
      , str
      , "<<<< parse error >>>>"
      , show err
      ]


-- | better than grep
parseExecStdout :: Parser a -> Command -> Args -> Craft a
parseExecStdout parser cmd args = do
  r <- exec cmd args
  s <- $stdoutOrError r
  parseExecResult r parser s


-- TODO: XXX: What if the file doesn't exist?
parseFile :: Parser a -> AbsFilePath -> Craft a
parseFile parser fp = do
  str <- B8.unpack <$> fileRead fp
  case runParser parser (show fp) str of
    Right x  -> return x
    Left err -> $craftError $ "parseFile `"++show fp++"` failed: "++show err


craftExecPath :: CraftEnv -> Craft [AbsDirPath]
craftExecPath ce = do
  let dps = maybe [] (splitOn ":") . Map.lookup "PATH" $ ce^.craftExecEnvVars
  mapM parseAbsDir dps


-- TODO: TESTME
prependPath :: AbsDirPath -> Craft a -> Craft a
prependPath newpath go = do
  ce <- ask
  execpath <- craftExecPath ce
  withPath (newpath:execpath) go


-- TODO: TESTME
withPath :: [AbsDirPath] -> Craft a -> Craft a
withPath = withEnvVar "PATH" . intercalate ":" . map fromAbsDir


withEnvVars :: ExecEnvVars -> Craft a -> Craft a
withEnvVars vars = local (\r -> r & craftExecEnvVars .~ vars)


withCWD :: AbsDirPath -> Craft a -> Craft a
withCWD dir = local (\r -> r & craftCWD .~ dir)


inDirectory :: Directory -> Craft a -> Craft a
inDirectory dir = withCWD $ dir^.directoryPath


-- TODO: TESTME
withEnvVar :: String -> String -> Craft a -> Craft a
withEnvVar name val go = do
  ce <- ask
  let vars = Map.insert name val $ ce ^. craftExecEnvVars
  withEnvVars vars go


withUser :: User -> Craft a -> Craft a
withUser user = local (\r -> r & craftUserID .~ user^.uid)


setMode :: Mode -> Path b t -> Craft ()
setMode m fp = exec_ "chmod" [toOctalString m, toFilePath fp]

setOwnerID :: UserID -> Path b t -> Craft ()
setOwnerID (UserID i) fp = exec_ "chown" [show i, toFilePath fp]


setGroupID :: GroupID -> Path b t -> Craft ()
setGroupID (GroupID i) fp = exec_ "chgrp" [show i, toFilePath fp]


stat :: Command
stat = "stat"

getMode :: Path Abs t -> Craft Mode
getMode fp = parseExecStdout modeParser stat ["-c", "%a", toFilePath fp]


getOwnerID :: Path Abs t -> Craft UserID
getOwnerID fp = UserID <$> parseExecStdout digitParser stat ["-c", "%u", toFilePath fp]


getGroupID :: Path Abs t -> Craft GroupID
getGroupID fp = GroupID <$> parseExecStdout digitParser stat ["-c", "%g", toFilePath fp]


getStats :: Path Abs t -> Craft (Maybe (Mode, UserID, GroupID))
getStats fp =
  exec stat ["-c", "%a:%u:%g", toFilePath fp] >>= \case
    Failure _ -> return Nothing
    Success r -> Just <$> parseExecResult (Success r) statsParser (r^.stdout)


statsParser :: Parser (Mode, UserID, GroupID)
statsParser = do
  mode' <- modeParser
  void $ char ':'
  owner' <- UserID <$> digitParser
  void $ char ':'
  group' <- GroupID <$> digitParser
  return (mode', owner', group')


modeParser :: Parser Mode
modeParser = fromOctalString <$> some digitChar


digitParser :: Parser Int
digitParser = read <$> some digitChar
