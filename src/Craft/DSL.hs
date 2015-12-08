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
  logger <- asks craftLogger
  cwd <- asks craftExecCWD
  env <- asks craftExecEnv
  lift $ execF logger cwd env cmd args


exec_ :: Command -> Args -> Craft ()
exec_ cmd args = do
  logDebugNS "exec_" $ unwords (cmd:args)
  logger <- asks craftLogger
  cwd <- asks craftExecCWD
  env <- asks craftExecEnv
  lift $ execF_ logger cwd env cmd args


fileRead :: FilePath -> Craft BS.ByteString
fileRead fp = lift $ fileReadF fp


fileWrite :: FilePath -> BS.ByteString -> Craft ()
fileWrite fp content = lift $ fileWriteF fp content


readSourceFile :: FilePath -> Craft ByteString
readSourceFile fp = do
  fps <- asks craftSourcePaths
  lift $ readSourceFileF fps fp

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
execF :: LogFunc -> CWD -> ExecEnv -> Command -> Args -> Free CraftDSL ExecResult
execF logger cwd env cmd args = liftF $ Exec logger cwd env cmd args id


execF_ :: LogFunc -> CWD -> ExecEnv -> Command -> Args -> Free CraftDSL ()
execF_ logger cwd env cmd args = liftF $ Exec_ logger cwd env cmd args ()


fileReadF :: FilePath -> Free CraftDSL BS.ByteString
fileReadF fp = liftF $ FileRead fp id


fileWriteF :: FilePath -> BS.ByteString -> Free CraftDSL ()
fileWriteF fp content = liftF $ FileWrite fp content ()


readSourceFileF :: [FilePath] -> FilePath -> Free CraftDSL BS.ByteString
readSourceFileF fps fp = liftF $ ReadSourceFile fps fp id