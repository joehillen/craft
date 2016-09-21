module Craft.Run.SSH where

import           Control.Concurrent        (threadDelay)
import           Control.Exception.Lifted  (bracket)
import           Control.Lens
import           Control.Monad.Logger      (LoggingT)
import           Control.Monad.Reader
import qualified Control.Monad.Trans       as Trans
import qualified Data.ByteString.Char8     as B8
import           Data.List                 (intersperse)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe)
import           Data.String.Utils         (replace)
import           System.Directory
import           System.Process            (ProcessHandle)
import qualified System.Process            as Process
import qualified System.Process.ByteString as Proc.BS
import           System.Random             hiding (next)
import           System.Timeout

import           Craft.Helpers
import           Craft.Run.Internal
import           Craft.Types


data SSHEnv
  = SSHEnv
    { _sshKey         :: FilePath
    , _sshAddr        :: String
    , _sshPort        :: Int
    , _sshUser        :: String
    , _sshSudo        :: Bool
    , _sshControlPath :: Maybe FilePath
    , _sshOptions     :: [String]
    }
makeLenses ''SSHEnv


connStr :: Optic' (->) (Const String) SSHEnv String
connStr = to (\env -> concat [env ^. sshUser, "@", env ^. sshAddr])


sshEnv :: String -> FilePath -> SSHEnv
sshEnv addr key =
  SSHEnv
  { _sshAddr        = addr
  , _sshPort        = 22
  , _sshKey         = key
  , _sshUser        = "root"
  , _sshSudo        = True
  , _sshControlPath = Nothing
  , _sshOptions     = sshDefaultOptions
  }


sshDefaultOptions :: [String]
sshDefaultOptions =
  [ "UserKnownHostsFile=/dev/null"
  , "StrictHostKeyChecking=no"
  , "LogLevel=ERROR"
  ]


data Session
 = Session
   { _sessionMasterProcHandle :: ProcessHandle
   , _sessionMasterProc       :: Process.CreateProcess
   , _sessionControlPath      :: FilePath
   , _sessionEnv              :: SSHEnv
   , _sessionArgs             :: Args
   }
makeLenses ''Session


prependEachWith :: String -> [String] -> [String]
prependEachWith _    []   = []
prependEachWith flag opts = flag:(intersperse flag opts)


newSession :: SSHEnv -> IO Session
newSession env = do
  defaultControlPath <- (".craft-ssh-session-" ++) . show . abs
                        <$> (randomIO :: IO Int)
  let controlPath = fromMaybe defaultControlPath $ env ^. sshControlPath
  let args =    [ "-p", show $ env ^. sshPort ] -- port
             ++ [ "-i", env ^. sshKey ] -- private key
             ++ prependEachWith "-o"
                  ((env ^. sshOptions)
                   ++ [ "ControlPath=" ++ controlPath
                      , "BatchMode=yes" -- never prompt for a password
                      ])
  let masterProc = (Process.proc "ssh"
                      (args
                       ++ (prependEachWith "-o" [ "ControlMaster=yes"
                                                , "ControlPersist=yes" ])
                       ++ [env ^. connStr]))
                    { Process.std_in  = Process.NoStream
                    , Process.std_out = Process.NoStream
                    , Process.std_err = Process.NoStream
                    }
  (_, _, _, !ph) <- Process.createProcess masterProc
  void $ timeout 10000000 {- 10 seconds -} $
    waitForControlPath controlPath
  return Session { _sessionEnv = env
                 , _sessionMasterProc = masterProc
                 , _sessionMasterProcHandle = ph
                 , _sessionControlPath = controlPath
                 , _sessionArgs = args
                 }


waitForControlPath :: FilePath -> IO ()
waitForControlPath controlPath = do
  !exists <- doesFileExist controlPath
  if exists
    then return ()
    else do
      threadDelay 100000 -- 100ms
      waitForControlPath controlPath


closeSession :: Session -> IO ()
closeSession Session{..} = do
  Process.terminateProcess _sessionMasterProcHandle
  whenM (doesFileExist _sessionControlPath) $
    removeFile _sessionControlPath


withSession :: SSHEnv -> (Session -> LoggingT IO a) -> LoggingT IO a
withSession env =
  bracket (Trans.lift $ newSession env)
          (Trans.lift . closeSession)


runCraftSSHSession :: Session -> CraftEnv -> Craft a -> LoggingT IO a
runCraftSSHSession session ce = interpretCraft ce $ runSSHFree session


-- | runCraftSSH
runCraftSSH :: SSHEnv -> CraftEnv -> Craft a -> LoggingT IO a
runCraftSSH env ce prog =
  withSession env $ \session -> runCraftSSHSession session ce prog


runSSHFree :: Session -> CraftDSL (LoggingT IO a) -> LoggingT IO a
runSSHFree session (Exec ce command args next) = do
  let p = sshProc session ce command args
  execProc p next
runSSHFree session (Exec_ ce command args next) = do
  let p = sshProc session ce command args
  execProc_ (unwords (command:args)) p next
runSSHFree session (FileRead ce fp next) = do
  let ceOverride = ce & craftExecEnv .~ Map.empty
                      & craftExecCWD .~ "/"
      p = sshProc session ceOverride "cat" [fp]
  (ec, content, stderr') <-
    Trans.lift $ Proc.BS.readCreateProcessWithExitCode p ""
  unless (isSuccessCode ec) $
    $craftError $ "Failed to read file '"++ fp ++"': " ++ B8.unpack stderr'
  next content
runSSHFree session (FileWrite ce fp content next) = do
  let ceOverride = ce & craftExecEnv .~ Map.empty
                      & craftExecCWD .~ "/"
      p = sshProc session ceOverride "tee" [fp]
  (ec, _, stderr') <-
    Trans.lift $ Proc.BS.readCreateProcessWithExitCode p content
  unless (isSuccessCode ec) $
    $craftError $ "Failed to write file '" ++ fp ++ "': " ++ B8.unpack stderr'
  next
runSSHFree session (SourceFile _ src dest next) = do
  let p = Process.proc "rsync"
            (   [ "--quiet" -- suppress non-error messages
                , "--checksum" -- skip based on checksum, not mod-time & size
                , "--compress" -- compress file data during the transfer
                  -- specify the remote shell to use
                , "--rsh=ssh " ++ unwords (session ^. sessionArgs)]
             ++ (if session ^. sessionEnv . sshSudo
                     then ["--super", "--rsync-path=sudo rsync"]
                     else [])
             ++ [ src , (session ^. sessionEnv . connStr) ++ ":" ++ dest ])
  execProc_ (showProc p) p next
runSSHFree _ (FindSourceFile ce name next) =
  Trans.lift (findSourceFileIO ce name) >>= next
runSSHFree _ (ReadSourceFile _ fp next) =
  Trans.lift (readSourceFileIO fp) >>= next


sshProc :: Session -> CraftEnv -> Command -> Args
        -> Process.CreateProcess
sshProc session ce command args =
  Process.proc "ssh" $ session ^. sessionArgs
                    ++ (prependEachWith "-o" [ "ControlMaster=auto"
                                             , "ControlPersist=no"
                                             ])
                    ++ [ session ^. sessionEnv . connStr
                       , fullExecStr
                       ]
 where
  fullExecStr :: String
  fullExecStr = unwords (sudoArgs ++ ["sh", "-c", "'", shellStr, "'"])

  sudoArgs :: [String]
  sudoArgs = if session ^. sessionEnv . sshSudo
              then ["sudo", "-n", "-H"]
              else []

  shellStr :: String
  shellStr = unwords (cdArgs ++ execEnvArgs ++ (command : map (escape specialChars) args))

  specialChars :: [String]
  specialChars = [" ", "*", "$", "'"]

  execEnvArgs :: [String]
  execEnvArgs = map (escape specialChars) . renderEnv $ ce ^. craftExecEnv

  cdArgs :: [String]
  cdArgs = ["cd", ce ^. craftExecCWD, ";"]

  escape :: [String] -> String -> String
  escape = recur backslash

  recur _ []     s = s
  recur f (a:as) s = recur f as $ f a s

  backslash x = replace x ('\\':x)


renderEnv :: ExecEnv -> [String]
renderEnv = map (\(k, v) -> k++"="++v) . Map.toList
