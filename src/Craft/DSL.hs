module Craft.DSL where

import Control.Monad.Reader
import Control.Monad.Free
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Megaparsec

import Craft.Types
import Craft.Helpers
import Craft.Log


-- | Craft DSL
exec :: Command -> Args -> Craft ExecResult
exec cmd args = do
  logDebugNS "exec" $ unwords (cmd:args)
  ce <- ask
  lift $ execF ce cmd args


exec_ :: Command -> Args -> Craft ()
exec_ cmd args = do
  logDebugNS "exec_" $ unwords (cmd:args)
  ce <- ask
  lift $ execF_ ce cmd args


fileRead :: FilePath -> Craft BS.ByteString
fileRead fp = do
  ce <- ask
  lift $ fileReadF ce fp


fileWrite :: FilePath -> BS.ByteString -> Craft ()
fileWrite fp content = do
  ce <- ask
  lift $ fileWriteF ce fp content


readSourceFile :: FilePath -> Craft ByteString
readSourceFile fp = do
  ce <- ask
  lift $ readSourceFileF ce fp

-- | better than grep
parseExecResult :: ExecResult -> Parsec String a -> String -> a
parseExecResult execr parser str =
  case parse parser (showProc $ execResultProc execr) str of
    Right x -> x
    Left err ->
      error $ concatMap appendNL
        [ "parseExecResult error:"
        , "<<<< process >>>>"
        , showProc $ execResultProc execr
        , "<<<< output >>>>"
        , str
        , "<<<< parse error >>>>"
        , show err
        ]


craftExecPath :: CraftEnv a -> [FilePath]
craftExecPath = maybe [] (splitOn ":") . lookup "PATH" . craftExecEnv


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
withEnv env = local (\r -> r { craftExecEnv = env })


-- TESTME
withEnvVar :: String -> String -> Craft a -> Craft a
withEnvVar name val go = do
  env <- asks craftExecEnv
  let env' = map (replaceKey name val) env
  withEnv env' go


isExecSucc :: ExecResult -> Bool
isExecSucc (ExecSucc _) = True
isExecSucc (ExecFail _) = False


errorOnFail :: ExecResult -> SuccResult
errorOnFail (ExecSucc r) = r
errorOnFail (ExecFail r) = error $ show r


-- | Free CraftDSL functions
execF :: CraftEnv pm  -> Command -> Args -> Free (CraftDSL pm) ExecResult
execF ce cmd args = liftF $ Exec ce cmd args id


execF_ :: CraftEnv pm -> Command -> Args -> Free (CraftDSL pm) ()
execF_ ce cmd args = liftF $ Exec_ ce cmd args ()


fileReadF :: CraftEnv pm -> FilePath -> Free (CraftDSL pm) BS.ByteString
fileReadF ce fp = liftF $ FileRead ce fp id


fileWriteF :: CraftEnv pm -> FilePath -> BS.ByteString -> Free (CraftDSL pm) ()
fileWriteF ce fp content = liftF $ FileWrite ce fp content ()


readSourceFileF :: CraftEnv pm -> FilePath -> Free (CraftDSL pm) BS.ByteString
readSourceFileF ce fp = liftF $ ReadSourceFile ce fp id
