module Craft.Run.SSH where

import Control.Exception (finally)
import Control.Monad.Free
import Control.Monad.Reader
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
    , sshPort    :: Int
    , sshUser    :: String
    , sshSudo    :: Bool
    , sshOptions :: [String]
    }


sshEnv :: String -> FilePath -> SSHEnv
sshEnv addr key =
  SSHEnv
  { sshAddr = addr
  , sshPort = 22
  , sshKey  = key
  , sshUser = "root"
  , sshSudo = True
  , sshOptions = sshDefaultOptions
  }

sshDefaultOptions :: [String]
sshDefaultOptions =
  [ "UserKnownHostsFile=/dev/null"
  , "StrictHostKeyChecking=no"
  , "LogLevel=ERROR"
  ]

data SSHSession
 = SSHSession
   { sshControlPath :: String
   }


newSSHSession :: IO SSHSession
newSSHSession = do
  randInt <- abs <$> randomIO :: IO Int
  return . SSHSession $ ".craft-ssh-session-" ++ show randInt


-- | runCraftSSH
runCraftSSH :: SSHEnv -> CraftEnv pm -> ReaderT (CraftEnv pm) (Free (CraftDSL pm)) a -> IO a
runCraftSSH sshenv ce prog = do
  sshSession <- newSSHSession
  let controlpath = sshControlPath sshSession
  finally
    (iterM (runCraftSSH' sshenv sshSession) . flip runReaderT ce $ prog)
    (whenM (doesFileExist controlpath) $
      removeFile controlpath)


-- | runCraftSSH implementation
runCraftSSH' :: SSHEnv -> SSHSession -> (CraftDSL pm) (IO a) -> IO a
runCraftSSH' sshenv sshsession (Exec ce command args next) = do
  let p = sshProc sshenv sshsession ce command args
  execProc ce p next

runCraftSSH' sshenv sshsession (Exec_ ce command args next) = do
  let p = sshProc sshenv sshsession ce command args
  execProc_ ce (unwords (command:args)) p next

runCraftSSH' sshenv sshsession (FileRead ce fp next) = do
  let ce' = ce { craftExecEnv = []
               , craftExecCWD = "/"
               }
      p = sshProc sshenv sshsession ce' "cat" [fp]
  (ec, content, stderr) <- liftIO $ Proc.BS.readCreateProcessWithExitCode p ""
  unless (isSuccess ec) $
    error $ "Failed to read file '"++ fp ++"': " ++ B8.unpack stderr
  next content

runCraftSSH' sshenv sshsession (FileWrite ce fp content next) = do
  let ce' = ce { craftExecEnv = []
               , craftExecCWD = "/"
               }
      p = sshProc sshenv sshsession ce' "tee" [fp]
  (ec, _, stderr) <- liftIO $ Proc.BS.readCreateProcessWithExitCode p content
  unless (isSuccess ec) $
    error $ "Failed to write file '"++ fp ++"': " ++ B8.unpack stderr
  next

runCraftSSH' sshenv sshsession (SourceFile ce src dest next) = do
  src' <- findSourceFile ce src
  let p = proc "rsync"
            ([ "--quiet" -- suppress non-error messages
             , "--checksum" -- skip based on checksum, not mod-time & size
             , "--compress" -- compress file data during the transfer
               -- specify the remote shell to use
             , "--rsh=ssh " ++ unwords (sshArgs sshenv sshsession)
             ] ++
             (if sshSudo sshenv then ["--super", "--rsync-path=sudo rsync"]
                                else [])
             ++
             [ src'
             , sshUser sshenv ++ "@" ++ sshAddr sshenv ++ ":" ++ dest
             ])
  execProc_ ce (showProc p) p next

runCraftSSH' _ _ (ReadSourceFile ce fp next) = readSourceFileIO ce fp >>= next
runCraftSSH' _ _ (Log ce loc logsource level logstr next) =
  let logger = craftLogger ce
  in logger loc logsource level logstr >> next


sshArgs :: SSHEnv -> SSHSession -> Args
sshArgs SSHEnv{..} SSHSession{..} =
  [ "-p", show sshPort ] ++
  [ "-i", sshKey ] ++
  ["-o" | not (null sshOptions)] ++
  intersperse "-o" sshOptions ++
  [ "-o", "ControlMaster=auto"
  , "-o", "ControlPath=" ++ sshControlPath
  , "-o", "ControlPersist=10"
  , "-o", "BatchMode=yes" -- never prompt for a password
  ]

sshProc :: SSHEnv -> SSHSession -> CraftEnv pm -> Command -> Args -> CreateProcess
sshProc sshenv sshsession ce command args =
  proc "ssh" (sshArgs sshenv sshsession
              ++ [ sshUser sshenv ++ "@" ++ sshAddr sshenv
                 , unwords cmd])
 where
  cwd = craftExecCWD ce
  env = craftExecEnv ce
  cmd = sudo ++
        ["sh", "-c", "'", "cd", escape cwd, ";"] ++
        map escape (renderEnv env) ++ (command : map escape args) ++
        ["'"]
  sudo = if sshSudo sshenv then ["sudo", "-H"] else []
  escape :: String -> String
  escape = recur backslash [" ", "*", "$", "'"]
  recur _ []     s = s
  recur f (a:as) s = recur f as $ f a s
  backslash x = replace x ('\\':x)


renderEnv :: ExecEnv -> [String]
renderEnv = map (\(k, v) -> k ++ "=" ++ v)
