module Craft.Exec where

import           Control.Monad.Reader
import           Control.Monad.Free
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           System.FilePath
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           System.Directory
import           System.Exit
import           System.Process hiding ( readCreateProcessWithExitCode
                                       , readProcessWithExitCode)
import           System.Process.ListLike
import qualified System.Process.ByteString as Proc.BS
import           Text.Megaparsec

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

readSourceFile :: FilePath -> Craft ByteString
readSourceFile fp = do
  fps <- asks craftSourcePaths
  lift $ readSourceFileF fps fp

-- | better than grep
parseExec :: Parsec String a -> Command -> Args -> Craft a
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


withEnv :: ExecEnv -> Craft a -> Craft a
withEnv env = local (\r -> r { craftExecEnv = env })


-- TESTME
withEnvVar :: String -> String -> Craft a -> Craft a
withEnvVar name val go = notImplemented "withEnvVar"


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
  let p = localProc env command args
  msg "exec" $ unwords [command, unwords args]
  (exit', stdout', stderr') <- readCreateProcessWithExitCode p "" {- stdin -}
  next $ ExecResult exit' stdout' stderr'

runCraftLocal' (Exec_ env command args next) = do
  msg "exec_" $ unwords [command, unwords args]
  let p = localProc env command args
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

runCraftLocal' (ReadSourceFile fps fp next) = do
  content <- readSourceFileIO fps fp
  next content

localProc :: ExecEnv -> Command -> Args -> CreateProcess
localProc env prog args =
  (proc prog args) { env           = Just env
                   , close_fds     = True
                   , create_group  = True
                   , delegate_ctlc = False
                   }

-- | Free CraftDSL functions
execF :: ExecEnv -> Command -> Args -> Free CraftDSL ExecResult
execF env cmd args = liftF $ Exec env cmd args id

execF_ :: ExecEnv -> Command -> Args -> Free CraftDSL ()
execF_ env cmd args = liftF $ Exec_ env cmd args ()

fileReadF :: FilePath -> Free CraftDSL BS.ByteString
fileReadF fp = liftF $ FileRead fp id

readSourceFileF :: [FilePath] -> FilePath -> Free CraftDSL BS.ByteString
readSourceFileF fps fp = liftF $ ReadSourceFile fps fp id

fileWriteF :: FilePath -> BS.ByteString -> Free CraftDSL ()
fileWriteF fp content = liftF $ FileWrite fp content ()


data SSHEnv
  = SSHEnv
    { sshEnvUser :: String
    , sshEnvKey  :: FilePath
    , sshEnvAddr :: String
    , sshSudo    :: Bool
    }

-- | runCraftSSH
runCraftSSH :: SSHEnv -> r -> ReaderT r (Free CraftDSL) a -> IO a
runCraftSSH sshEnv e = iterM (runCraftSSH' sshEnv) . flip runReaderT e

-- | runCraftSSH implementation
runCraftSSH' :: SSHEnv -> CraftDSL (IO a) -> IO a
runCraftSSH' sshEnv (Exec env command args next) = do
  let p = sshProc sshEnv env command args
  msg "exec" $ showProc p
  (exit', stdout', stderr') <- readCreateProcessWithExitCode p "" {- stdin -}
  next $ ExecResult exit' stdout' stderr'

runCraftSSH' sshEnv (Exec_ env command args next) = do
  let p = sshProc sshEnv env command args
  msg "exec_" $ showProc p
  (_, _, _, ph) <- liftIO $ createProcess p
  liftIO (waitForProcess ph) >>= \case
    ExitFailure n -> error $ "exec_ failed with code: " ++ show n
    ExitSuccess   -> next

runCraftSSH' sshEnv (FileRead fp next) = do
  let p = sshProc sshEnv [] "cat" [fp]
  msg "exec_" $ showProc p
  (ec, content, stderr) <- liftIO $ Proc.BS.readCreateProcessWithExitCode p ""
  unless (isSuccess ec) $
    error $ "Failed to read file '"++ fp ++"': " ++ B8.unpack stderr
  next content

runCraftSSH' sshEnv (FileWrite fp content next) = do
  let p = sshProc sshEnv [] "tee" [fp]
  msg "exec_" $ showProc p
  (ec, _, stderr) <- liftIO $ Proc.BS.readCreateProcessWithExitCode p content
  unless (isSuccess ec) $
    error $ "Failed to write file '"++ fp ++"': " ++ B8.unpack stderr
  next

runCraftSSH' _ (ReadSourceFile fps fp next) = do
  content <- readSourceFileIO fps fp
  next content


showProc :: CreateProcess -> String
showProc p =
  case cmdspec p of
    ShellCommand s -> s
    RawCommand fp args -> unwords [fp, unwords args]


sshProc :: SSHEnv -> ExecEnv -> Command -> Args -> CreateProcess
sshProc SSHEnv{..} env command args =
  proc "ssh" ([ "-i", sshEnvKey, sshEnvUser ++ "@" ++ sshEnvAddr
              , unwords cmd])
 where
  cmd = sudo ++ map quote (renderEnv env) ++ command : map quote args
  sudo = if sshSudo then ["sudo"] else []
  quote "" = "\"\""
  quote v
    | ' ' `elem` v = "\"" ++ v ++ "\""
    | otherwise    = v

renderEnv :: ExecEnv -> [String]
renderEnv = map (\(k, v) -> k ++ "=" ++ v)

readSourceFileIO :: [FilePath] -> FilePath -> IO BS.ByteString
readSourceFileIO fps name = do
  files <- filterM (\fp -> doesFileExist $ fp </> name) fps
  if null files then
    error $ "Source file `" ++ name ++ "` not found in file sources: "
            ++ show fps
  else
    BS.readFile $ head files </> name
