module Craft.DSL where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Free
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import           Control.Monad.Logger (logDebugNS)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Text.Megaparsec
import           System.FilePath ((</>))

import           Craft.Types
import           Craft.Helpers


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


parseExecResult :: ExecResult -> Parsec String a -> String -> Craft a
parseExecResult execr parser str =
  case parse parser (showProc $ execResultProc execr) str of
    Right x -> return x
    Left err ->
      $craftError $ concatMap appendNL
        [ "parseExecResult error:"
        , "<<<< process >>>>"
        , showProc $ execResultProc execr
        , "<<<< output >>>>"
        , str
        , "<<<< parse error >>>>"
        , show err
        ]


-- | better than grep
parseExecStdout :: Parsec StdOut a -> Command -> Args -> Craft a
parseExecStdout parser cmd args = do
  r <- exec cmd args
  s <- $stdoutOrError r
  parseExecResult r parser s


craftExecPath :: CraftEnv -> [FilePath]
craftExecPath = maybe [] (splitOn ":") . Map.lookup "PATH" . view craftExecEnv


-- TESTME
prependPath :: FilePath -> Craft a -> Craft a
prependPath newpath go = do
  path <- asks craftExecPath
  withPath (newpath:path) go


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


-- | Free CraftDSL functions
execF :: CraftEnv -> Command -> Args -> Free CraftDSL ExecResult
execF ce cmd args = liftF $ Exec ce cmd args id


execF_ :: CraftEnv -> Command -> Args -> Free CraftDSL ()
execF_ ce cmd args = liftF $ Exec_ ce cmd args ()


fileReadF :: CraftEnv -> FilePath -> Free CraftDSL BS.ByteString
fileReadF ce fp = liftF $ FileRead ce fp id


fileWriteF :: CraftEnv -> FilePath -> BS.ByteString -> Free CraftDSL ()
fileWriteF ce fp content = liftF $ FileWrite ce fp content ()


sourceFileF :: CraftEnv -> FilePath -> FilePath -> Free CraftDSL ()
sourceFileF ce src dest = liftF $ SourceFile ce src dest ()


findSourceFileF :: CraftEnv -> FilePath -> Free CraftDSL [FilePath]
findSourceFileF ce name = liftF $ FindSourceFile ce name id


readSourceFileF :: CraftEnv -> FilePath -> Free CraftDSL BS.ByteString
readSourceFileF ce fp = liftF $ ReadSourceFile ce fp id
