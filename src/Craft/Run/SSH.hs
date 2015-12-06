module Craft.Run.SSH where

import Control.Exception (finally)
import Control.Monad.Free
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.List (intersperse)
import Data.String.Utils (replace)
import System.Directory
import System.Process
import System.Random hiding (next)
import qualified System.Process.ByteString as Proc.BS

import Craft.Types
import Craft.Helpers
import Craft.Run.Internal

data SSHEnv
  = SSHEnv
    { sshKey     :: FilePath
    , sshAddr    :: String
    , sshUser    :: String
    , sshSudo    :: Bool
    , sshOptions :: [String]
    }


sshEnv :: String -> FilePath -> SSHEnv
sshEnv addr key =
  SSHEnv
  { sshAddr = addr
  , sshKey  = key
  , sshUser = "root"
  , sshSudo = True
  , sshOptions = [ "UserKnownHostsFile=/dev/null"
                 , "StrictHostKeyChecking=no"
                 , "LogLevel=ERROR"
                 ]
  }

data SSHSession
 = SSHSession
   { sshControlPath :: String
   }


newSSHSession :: IO SSHSession
newSSHSession = do
  randInt <- randomIO :: IO Int
  return . SSHSession $ ".craft-ssh-session-" ++ show randInt


-- | runCraftSSH
runCraftSSH :: forall pm. PackageManager pm
            => forall a. SSHEnv -> CraftEnv pm -> Craft a -> IO a
runCraftSSH sshenv craftenv prog = do
  sshSession <- newSSHSession
  let controlpath = sshControlPath sshSession
  finally
    (iterM (runCraftSSH' sshenv sshSession) . flip runReaderT craftenv $ prog)
    (whenM (doesFileExist controlpath) $
      removeFile controlpath)


-- | runCraftSSH implementation
runCraftSSH' :: SSHEnv -> SSHSession -> CraftDSL pm (IO a) -> IO a
runCraftSSH' sshenv SSHSession{..} (Exec craftenv command args next) = do
  let p = sshProc sshenv sshControlPath craftenv command args
  execProc p next

runCraftSSH' sshenv SSHSession{..} (Exec_ craftenv command args next) = do
  let p = sshProc sshenv sshControlPath craftenv command args
  execProc_ p next

runCraftSSH' sshenv SSHSession{..} (FileRead fp next) = do
  let p = sshProc sshenv sshControlPath craftEnv "/bin/cat" [fp]
  (ec, content, stderr) <- liftIO $ Proc.BS.readCreateProcessWithExitCode p ""
  unless (isSuccess ec) $
    error $ "Failed to read file '"++ fp ++"': " ++ B8.unpack stderr
  next content

runCraftSSH' sshenv SSHSession{..} (FileWrite fp content next) = do
  let p = sshProc sshenv sshControlPath craftEnv "/bin/tee" [fp]
  (ec, _, stderr) <- liftIO $ Proc.BS.readCreateProcessWithExitCode p content
  unless (isSuccess ec) $
    error $ "Failed to write file '"++ fp ++"': " ++ B8.unpack stderr
  next

runCraftSSH' _ _ (ReadSourceFile fps fp next) = readSourceFileIO fps fp >>= next
runCraftSSH' _ _ (Log h bs next) = BS.hPutStr h bs >> next



sshProc :: SSHEnv -> FilePath -> CraftEnv pm -> Command -> Args -> CreateProcess
sshProc SSHEnv{..} controlpath craftenv command args =
  proc "ssh" ([ "-i", sshKey ] ++
              ["-o" | not (null sshOptions)] ++
              intersperse "-o" sshOptions ++
              [ "-o", "ControlMaster=auto"
              , "-o", "ControlPath=" ++ controlpath
              , "-o", "ControlPersist=10"
              ] ++
              ["-o", "BatchMode=yes"] ++ -- never prompt for a password
              [ sshUser ++ "@" ++ sshAddr
              , unwords cmd])
 where
  cwd = craftExecCWD craftenv
  env = craftExecEnv craftenv
  cmd = sudo ++
        ["sh", "-c", "'", "cd", escape cwd, ";"] ++
        map escape (renderEnv env) ++ (command : map escape args) ++
        ["'"]
  sudo = ["sudo" | sshSudo ] ++ [ "-H" | sshSudo ]
  escape :: String -> String
  escape = recur backslash [" ", "*", "$", "'"]
  recur _ []     s = s
  recur f (a:as) s = recur f as $ f a s
  backslash x = replace x ('\\':x)


renderEnv :: ExecEnv -> [String]
renderEnv = map (\(k, v) -> k ++ "=" ++ v)
