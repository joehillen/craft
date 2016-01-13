module Craft.DSL where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Free
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Megaparsec
import System.FilePath ((</>))

import Craft.Types
import Craft.Helpers
import Craft.Log


-- | Craft DSL
exec :: Command -> Args -> Craft ExecResult
exec cmd args = do
  logDebugNS "exec" $ unwords (cmd:args)
  ce <- ask
  lift . lift $ execF ce cmd args


exec_ :: Command -> Args -> Craft ()
exec_ cmd args = do
  logDebugNS "exec_" $ unwords (cmd:args)
  ce <- ask
  lift . lift $ execF_ ce cmd args


fileRead :: FilePath -> Craft BS.ByteString
fileRead fp = do
  ce <- ask
  lift . lift $ fileReadF ce fp


fileWrite :: FilePath -> BS.ByteString -> Craft ()
fileWrite fp content = do
  ce <- ask
  lift . lift $ fileWriteF ce fp content


sourceFile :: FilePath -> FilePath -> Craft ()
sourceFile name dest = do
  ce <- ask
  fp <- findSourceFile name
  lift . lift $ sourceFileF ce fp dest


findSourceFile :: FilePath -> Craft FilePath
findSourceFile name = do
  ce <- ask
  let fps = ce ^. craftSourcePaths
  files <- lift . lift $ findSourceFileF ce name
  if null files then
    $craftError $ "Source file `" ++ name ++ "` not found in file sources: "
                   ++ show fps
  else
    return $ head files </> name


readSourceFile :: FilePath -> Craft ByteString
readSourceFile name = do
  ce <- ask
  fp <- findSourceFile name
  lift . lift $ readSourceFileF ce fp


-- | better than grep
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


craftExecPath :: CraftEnv -> [FilePath]
craftExecPath = maybe [] (splitOn ":") . lookup "PATH" . view craftExecEnv


-- TESTME
prependPath :: FilePath -> Craft a -> Craft a
prependPath newpath go = do
  path <- asks craftExecPath
  withPath (newpath:path) go


-- TESTME
withPath :: [FilePath] -> Craft a -> Craft a
withPath = withEnvVar "PATH" . intercalate ":"


replaceKey :: Eq a => a -> b -> (a, b) -> (a, b)
replaceKey k'  v' (k, v)
  | k == k'   = (k, v')
  | otherwise = (k, v)


withEnv :: ExecEnv -> Craft a -> Craft a
withEnv env = local (\r -> r & craftExecEnv .~ env)


-- TESTME
withEnvVar :: String -> String -> Craft a -> Craft a
withEnvVar name val go = do
  ce <- ask
  let env = map (replaceKey name val) $ ce ^. craftExecEnv
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
