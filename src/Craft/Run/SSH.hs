module Craft.Run.SSH where

import           Control.Concurrent        (threadDelay)
import           Control.Exception.Lifted  (bracket)
import           Control.Lens
import           Control.Monad.Logger      (LoggingT, logDebugNS)
import           Control.Monad.Reader
import qualified Control.Monad.Trans       as Trans
import qualified Data.ByteString.Char8     as B8
import           Data.List                 (intersperse)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe)
import           Data.String.Utils         (replace)
import qualified Data.Text                 as T
import           Formatting
import           Formatting.ShortFormatters
import qualified Path
import           Path.IO
import           System.Process            (ProcessHandle)
import qualified System.Process            as Process
import qualified System.Process.ByteString as Proc.BS
import           System.Random             hiding (next)
import           System.Timeout

import           Craft.Helpers
import           Craft.Run.Internal
import           Craft.Types


data SSHEnv = SSHEnv
  { _sshKey         :: AbsFilePath
  , _sshAddr        :: String
  , _sshPort        :: Int
  , _sshUser        :: String
  , _sshControlPath :: Maybe RelFilePath
  , _sshOptions     :: [String]
  }
makeLenses ''SSHEnv


connectionString :: Optic' (->) (Const String) SSHEnv String
connectionString = to (\env -> concat [env ^. sshUser, "@", env ^. sshAddr])


sshEnv :: String -> AbsFilePath -> SSHEnv
sshEnv addr key =
  SSHEnv
  { _sshAddr        = addr
  , _sshPort        = 22
  , _sshKey         = key
  , _sshUser        = "root"
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
   , _sessionMasterProcess    :: Process.CreateProcess
   , _sessionControlPath      :: RelFilePath
   , _sessionEnv              :: SSHEnv
   , _sessionArgs             :: Args
   }
makeLenses ''Session


prependEachWith :: String -> [String] -> [String]
prependEachWith _    []   = []
prependEachWith flag opts = flag:(intersperse flag opts)


newSession :: SSHEnv -> IO Session
newSession env = do
  randint <- abs <$> (randomIO :: IO Int)
  let defaultControlPathFN =
        formatToString (".craft-ssh-session-"%s%":"%d%"-"%d)
          (env^.connectionString)
          (env^.sshPort)
          randint
  defaultControlPath <- parseRelFile defaultControlPathFN
  let controlPath = fromMaybe defaultControlPath (env^.sshControlPath)
  let args =    [ "-p", show $ env^.sshPort ] -- port
             ++ [ "-i", fromAbsFile $ env^.sshKey ] -- private key
             ++ prependEachWith "-o"
                  ((env ^. sshOptions)
                   ++ [ "ControlPath=" ++ (fromRelFile controlPath)
                      , "BatchMode=yes" -- never prompt for a password
                      ])
  let masterProc = (Process.proc "ssh"
                      (args
                       ++ (prependEachWith "-o" [ "ControlMaster=yes"
                                                , "ControlPersist=yes" ])
                       ++ [env ^. connectionString]))
                    { Process.std_in  = Process.NoStream
                    , Process.std_out = Process.NoStream
                    , Process.std_err = Process.NoStream
                    }
  (_, _, _, !ph) <- Process.createProcess masterProc
  void $ timeout 10000000 {- 10 seconds -} $
    waitForControlPath controlPath
  return
    Session
    { _sessionEnv              = env
    , _sessionMasterProcess    = masterProc
    , _sessionMasterProcHandle = ph
    , _sessionControlPath      = controlPath
    , _sessionArgs             = args
    }


waitForControlPath :: Path a Path.File -> IO ()
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


runSSHSession :: Session -> CraftRunner
runSSHSession session =
  CraftRunner
  { runExec =
      \ce command args ->
        runCreateProcess =<< sshProc session ce command args
  , runExec_ =
      \ce command args ->
        runCreateProcess_ (unwords (command:args)) =<< sshProc session ce command args
  , runFileRead =
      \fp -> do
        p <- sshProc session craftEnvOverride "cat" [fromAbsFile fp]
        (ec, content, stderr') <-
          Trans.lift $ Proc.BS.readCreateProcessWithExitCode p ""
        unless (isSuccessCode ec) $
          $craftError $ "Failed to read file '"++ fromAbsFile fp ++"': " ++ B8.unpack stderr'
        return content
  , runFileWrite =
      \fp content -> do
        p <- sshProc session craftEnvOverride "tee" [fromAbsFile fp]
        (ec, _, stderr') <-
          Trans.lift $ Proc.BS.readCreateProcessWithExitCode p content
        unless (isSuccessCode ec) $
          $craftError $ "Failed to write file '" ++ fromAbsFile fp ++ "': " ++ B8.unpack stderr'
  , runSourceFile =
      \src dest -> do
        let p =
              Process.proc "rsync"
                [ "--quiet" -- suppress non-error messages
                , "--checksum" -- skip based on checksum, not mod-time & size
                , "--compress" -- compress file data during the transfer
                -- specify the remote shell to use
                , "--rsh=ssh " ++ unwords (session ^. sessionArgs)
                , "--super"
                , "--rsync-path=sudo rsync"
                , src
                , (session^.sessionEnv.connectionString)++":"++fromAbsFile dest
                ]
        logDebugNS "ssh" $ T.pack $ showProcess p
        runCreateProcess_ (showProcess p) p
  }
 where
  craftEnvOverride :: CraftEnv
  craftEnvOverride =
    craftEnv noPackageManager
    & craftExecEnv .~ Map.empty
    & craftExecCWD .~ $(mkAbsDir "/")


runCraftSSH :: SSHEnv -> CraftEnv -> Craft a -> LoggingT IO a
runCraftSSH env ce configs =
  withSession env $ \session -> runCraft (runSSHSession session) ce configs


sshProc :: Session -> CraftEnv -> Command -> Args
        -> LoggingT IO Process.CreateProcess
sshProc session ce command args = do
  let p =
        Process.proc
          "ssh"
          $ (session^.sessionArgs)
            ++ (prependEachWith "-o"
                [ "ControlMaster=auto"
                , "ControlPersist=no"
                ])
            ++ [ session ^. sessionEnv . connectionString
               , fullExecStr
               ]
  logDebugNS "ssh" $ T.pack $ showProcess p
  return p
 where
  fullExecStr :: String
  fullExecStr = unwords (sudoArgs ++ ["sh", "-c", "'", shellStr, "'"])

  sudoArgs :: [String]
  sudoArgs =
    let UserID uid' = ce^.craftUserID in
    ["sudo", "-u", "\\#"++(show uid'), "-n", "-H"]

  shellStr :: String
  shellStr = unwords (cdArgs ++ execEnvArgs ++ (command : map (escape specialChars) args))

  specialChars :: [String]
  specialChars = [" ", "*", "$", "'"]

  execEnvArgs :: [String]
  execEnvArgs = map (escape specialChars) . renderEnv $ ce ^. craftExecEnv

  cdArgs :: [String]
  cdArgs = ["cd", (fromAbsDir $ ce^.craftExecCWD), ";"]

  escape :: [String] -> String -> String
  escape = recur backslash

  recur _   []     str = str
  recur fun (a:as) str = recur fun as $ fun a str

  backslash char' = replace char' ('\\':char')


renderEnv :: ExecEnv -> [String]
renderEnv = map (\(k, v) -> k++"="++v) . Map.toList
