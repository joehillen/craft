module Craft.DSL where

import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.Logger   (logDebugNS)
import           Control.Monad.Reader
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as B8
import           Data.List              (intercalate)
import           Data.List.Split        (splitOn)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import           System.FilePath        ((</>))
import           Text.Megaparsec
import           Text.Megaparsec.String

import           Craft.Types


-- | Craft DSL
exec :: Command -> Args -> Craft ExecResult
exec cmd args = do
  logDebugNS "exec" $ T.unwords $ map T.pack (cmd:args)
  ce <- ask
  liftF $ Exec ce cmd args id


exec_ :: Command -> Args -> Craft ()
exec_ cmd args = do
  logDebugNS "exec_" $ T.unwords $ map T.pack (cmd:args)
  ce <- ask
  liftF $ Exec_ ce cmd args ()


fileRead :: FilePath -> Craft BS.ByteString
fileRead fp = do
  ce <- ask
  liftF $ FileRead ce fp id


fileWrite :: FilePath -> BS.ByteString -> Craft ()
fileWrite fp content = do
  ce <- ask
  liftF $ FileWrite ce fp content ()


sourceFile :: FilePath -> FilePath -> Craft ()
sourceFile name dest = do
  ce <- ask
  fp <- findSourceFile name
  liftF $ SourceFile ce fp dest ()


findSourceFile :: FilePath -> Craft FilePath
findSourceFile name = do
  ce <- ask
  let fps = ce ^. craftSourcePaths
  files <- liftF $ FindSourceFile ce name id
  if null files then
    $craftError $ "Source file `" ++ name ++ "` not found in file sources: "
                   ++ show fps
  else
    return $ head files </> name


readSourceFile :: FilePath -> Craft ByteString
readSourceFile name = do
  ce <- ask
  fp <- findSourceFile name
  liftF $ ReadSourceFile ce fp id


parseExecResult :: ExecResult -> Parser a -> String -> Craft a
parseExecResult execr parser str =
  case parse parser (showProc $ execResultProc execr) str of
    Right x -> return x
    Left err -> $craftError $
      unlines
      [ "parseExecResult error:"
      , "<<<< process >>>>"
      , showProc $ execResultProc execr
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


-- XXX: What if the file doesn't exist?
parseFile :: Parser a -> FilePath -> Craft a
parseFile parser fp = do
  str <- B8.unpack <$> fileRead fp
  case runParser parser fp str of
    Right x  -> return x
    Left err -> $craftError $ "parseFile `"++fp++"` failed: "++show err


craftExecPath :: CraftEnv -> [FilePath]
craftExecPath = maybe [] (splitOn ":") . Map.lookup "PATH" . view craftExecEnv


-- TESTME
prependPath :: FilePath -> Craft a -> Craft a
prependPath newpath go = do
  execpath <- asks craftExecPath
  withPath (newpath:execpath) go


-- TESTME
withPath :: [FilePath] -> Craft a -> Craft a
withPath = withEnvVar "PATH" . intercalate ":"


withEnv :: ExecEnv -> Craft a -> Craft a
withEnv env = local (\r -> r & craftExecEnv .~ env)


-- TESTME
withEnvVar :: String -> String -> Craft a -> Craft a
withEnvVar name val go = do
  ce <- ask
  let env = Map.insert name val $ ce ^. craftExecEnv
  withEnv env go


isExecSucc :: ExecResult -> Bool
isExecSucc (ExecSucc _) = True
isExecSucc (ExecFail _) = False

setMode :: Mode -> FilePath -> Craft ()
setMode m fp = exec_ "chmod" [toOctalString m, fp]

setOwnerID :: UserID -> FilePath -> Craft ()
setOwnerID (UserID i) fp = exec_ "chown" [show i, fp]


setGroupID :: GroupID -> FilePath -> Craft ()
setGroupID (GroupID i) fp = exec_ "chgrp" [show i, fp]


stat :: Command
stat = "stat"

getMode :: FilePath -> Craft Mode
getMode fp = parseExecStdout modeParser stat ["-c", "%a", fp]


getOwnerID :: FilePath -> Craft UserID
getOwnerID fp = UserID <$> parseExecStdout digitParser stat ["-c", "%u", fp]


getGroupID :: FilePath -> Craft GroupID
getGroupID fp = GroupID <$> parseExecStdout digitParser stat ["-c", "%g", fp]


getStats :: FilePath -> Craft (Maybe (Mode, UserID, GroupID))
getStats fp =
  exec stat ["-c", "%a:%u:%g", fp] >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> Just <$> parseExecResult (ExecSucc r) statsParser (r ^. stdout)


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
