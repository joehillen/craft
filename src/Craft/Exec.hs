module Craft.Exec where

import           Control.Monad.Reader
import           Control.Monad.Free
import qualified Data.ByteString as BS
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           System.Exit
import           System.Process hiding ( readCreateProcessWithExitCode
                                       , readProcessWithExitCode)
import           System.Process.ListLike
import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Craft.Types
import           Craft.Helpers


-- | Craft DSL
exec :: Command -> Args -> Craft ExecResult
exec cmd args = do
  env <- asks craftExecEnv
  lift $ execF env cmd args

exec_ :: Command -> Args -> Craft ()
exec_ cmd args = do
  env <- asks craftExecEnv
  lift $ execF_ env cmd args

fileRead :: FilePath -> Craft BS.ByteString
fileRead fp = lift $ fileReadF fp

fileWrite :: FilePath -> BS.ByteString -> Craft ()
fileWrite fp content = lift $ fileWriteF fp content

-- | better than grep
parseExec :: Parser a -> Command -> Args -> Craft a
parseExec parser command args = do
  r <- exec command args
  let exitStr = case (exitcode r) of
                  ExitSuccess   -> "0"
                  ExitFailure c -> show c
  case parse parser (unwords $ command:args) (stdout r) of
    Right x -> return x
    Left err -> error $
      "parseExec failed!\n"
      ++ show err ++ "\n"
      ++ "stdout:\n"
      ++ (stdout r) ++ "\n" ++
      if not (null (stderr r)) then "stderr:\n"   ++ (stderr r) ++ "\n"
                           else ""
      ++ "exit code: " ++ exitStr


craftExecPath :: CraftEnv a -> [FilePath]
craftExecPath craftEnv =
  maybe [] (splitOn ":") $ lookup "PATH" $ craftExecEnv craftEnv

-- TESTME
prependPath :: FilePath -> Craft a -> Craft a
prependPath newpath go = do
  path <- asks craftExecPath
  withPath (newpath:path) go

-- TESTME
withPath :: [FilePath] -> Craft a -> Craft a
withPath paths go = do
  env <- asks craftExecEnv
  let env' = map (replaceKey "PATH" paths') env
  withEnv env' go
 where
  paths' = intercalate ":" paths


replaceKey :: Eq a => a -> b -> (a, b) -> (a, b)
replaceKey k'  v' (k, v)
  | k == k'   = (k, v')
  | otherwise = (k, v)


withEnv :: Env -> Craft a -> Craft a
withEnv env = local (\r -> r { craftExecEnv = env })


-- TESTME
withEnvVar :: String -> String -> Craft a -> Craft a
withEnvVar name val go = undefined


withCWD :: FilePath -> Craft a -> Craft a
withCWD path = local (\r -> r { craftExecCWD = path })


isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess     = True
isSuccess (ExitFailure _) = False

-- | runCraftLocal
runCraftLocal :: r -> ReaderT r (Free CraftDSL) a -> IO a
runCraftLocal e = iterM runCraftLocal' . flip runReaderT e

-- | runCraftLocal implementation
runCraftLocal' :: CraftDSL (IO a) -> IO a
runCraftLocal' (Exec env command args next) = do
  let p = craftProc env command args
  msg "exec" $ unwords [command, unwords args]
  (exit', stdout', stderr') <- readCreateProcessWithExitCode p "" {- stdin -}
  next $ ExecResult exit' stdout' stderr'
runCraftLocal' (Exec_ env command args next) = do
  msg "exec_" $ unwords [command, unwords args]
  let p = craftProc env command args
  (_, _, _, ph) <- liftIO $ createProcess p
  liftIO (waitForProcess ph) >>= \case
    ExitFailure n -> error $ "exec_ failed with code: " ++ show n
    ExitSuccess   -> next
runCraftLocal' (FileRead fp next) = do
  content <- BS.readFile fp
  next content
runCraftLocal' (FileWrite fp content next) = do
  BS.writeFile fp content
  next

craftProc :: Env -> Command -> Args -> CreateProcess
craftProc env prog args =
  (proc prog args) { env           = Just env
                   , close_fds     = True
                   , create_group  = True
                   , delegate_ctlc = False
                   }

-- | Free CraftDSL functions
execF :: Env -> Command -> Args -> Free CraftDSL ExecResult
execF env cmd args = liftF $ Exec env cmd args id

execF_ :: Env -> Command -> Args -> Free CraftDSL ()
execF_ env cmd args = liftF $ Exec_ env cmd args ()

fileReadF :: FilePath -> Free CraftDSL BS.ByteString
fileReadF fp = liftF $ FileRead fp id

fileWriteF :: FilePath -> BS.ByteString -> Free CraftDSL ()
fileWriteF fp content = liftF $ FileWrite fp content ()

