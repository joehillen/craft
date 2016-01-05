module Craft.Run.SSH where

import Control.Lens
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
import Craft.Log

data SSHEnv
  = SSHEnv
    { _sshKey     :: FilePath
    , _sshAddr    :: String
    , _sshPort    :: Int
    , _sshUser    :: String
    , _sshSudo    :: Bool
    , _sshOptions :: [String]
    }
makeLenses ''SSHEnv


sshEnv :: String -> FilePath -> SSHEnv
sshEnv addr key =
  SSHEnv
  { _sshAddr = addr
  , _sshPort = 22
  , _sshKey  = key
  , _sshUser = "root"
  , _sshSudo = True
  , _sshOptions = [ "UserKnownHostsFile=/dev/null"
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
  randInt <- abs <$> randomIO :: IO Int
  return . SSHSession $ ".craft-ssh-session-" ++ show randInt


-- | runCraftSSH
runCraftSSH :: SSHEnv -> CraftEnv -> ReaderT CraftEnv (Free CraftDSL) a -> IO a
runCraftSSH sshenv ce prog = do
  sshSession <- newSSHSession
  let controlpath = sshControlPath sshSession
  finally
    (iterM (runCraftSSH' sshenv sshSession) . flip runReaderT ce $ prog)
    (whenM (doesFileExist controlpath) $
      removeFile controlpath)


-- | runCraftSSH implementation
runCraftSSH' :: SSHEnv -> SSHSession -> CraftDSL (IO a) -> IO a
runCraftSSH' sshenv sshsession (Exec ce command args next) = do
  let p = sshProc sshenv sshsession ce command args
  execProc ce p next

runCraftSSH' sshenv sshsession (Exec_ ce command args next) = do
  let p = sshProc sshenv sshsession ce command args
  execProc_ ce (unwords (command:args)) p next

runCraftSSH' sshenv sshsession (FileRead ce fp next) = do
  let ce' = ce & craftExecEnv .~ []
               & craftExecCWD .~ "/"
      p = sshProc sshenv sshsession ce' "cat" [fp]
  (ec, content, stderr') <- liftIO $ Proc.BS.readCreateProcessWithExitCode p ""
  unless (isSuccess ec) $
    runCraftSSH sshenv ce $
      $craftError $ "Failed to read file '"++ fp ++"': " ++ B8.unpack stderr'
  next content

runCraftSSH' sshenv sshsession (FileWrite ce fp content next) = do
  let ce' = ce & craftExecEnv .~ []
               & craftExecCWD .~ "/"
      p = sshProc sshenv sshsession ce' "tee" [fp]
  (ec, _, stderr') <- liftIO $ Proc.BS.readCreateProcessWithExitCode p content
  unless (isSuccess ec) $
    runCraftSSH sshenv ce $
      $craftError $ "Failed to write file '"++ fp ++"': " ++ B8.unpack stderr'
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
             (if sshenv ^. sshSudo then ["--super", "--rsync-path=sudo rsync"]
                                   else [])
             ++
             [ src'
             , sshenv ^. sshUser ++ "@" ++ sshenv ^. sshAddr ++ ":" ++ dest
             ])
  execProc_ ce (showProc p) p next

runCraftSSH' _ _ (ReadSourceFile ce fp next) = readSourceFileIO ce fp >>= next
runCraftSSH' _ _ (Log ce loc logsource level logstr next) =
  (ce ^. craftLogger) loc logsource level logstr >> next


sshArgs :: SSHEnv -> SSHSession -> Args
sshArgs SSHEnv{..} SSHSession{..} =
  [ "-p", show _sshPort ] ++
  [ "-i", _sshKey ] ++
  ["-o" | not (null _sshOptions)] ++
  intersperse "-o" _sshOptions ++
  [ "-o", "ControlMaster=auto"
  , "-o", "ControlPath=" ++ sshControlPath
  , "-o", "ControlPersist=10"
  , "-o", "BatchMode=yes" -- never prompt for a password
  ]

sshProc :: SSHEnv -> SSHSession -> CraftEnv -> Command -> Args -> CreateProcess
sshProc sshenv sshsession ce command args =
  proc "ssh" (sshArgs sshenv sshsession
              ++ [ sshenv ^. sshUser ++ "@" ++ sshenv ^. sshAddr
                 , unwords cmd])
 where
  cwd = ce ^. craftExecCWD
  env = ce ^. craftExecEnv
  cmd = sudo ++
        ["sh", "-c", "'", "cd", escape cwd, ";"] ++
        map escape (renderEnv env) ++ (command : map escape args) ++
        ["'"]
  sudo = if sshenv ^. sshSudo then ["sudo", "-H"] else []
  escape :: String -> String
  escape = recur backslash [" ", "*", "$", "'"]
  recur _ []     s = s
  recur f (a:as) s = recur f as $ f a s
  backslash x = replace x ('\\':x)


renderEnv :: ExecEnv -> [String]
renderEnv = map (\(k, v) -> k ++ "=" ++ v)
