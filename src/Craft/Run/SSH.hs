{-# LANGUAGE BangPatterns #-}
module Craft.Run.SSH where

import           Control.Concurrent         (threadDelay)
import           Control.Exception.Lifted   (bracket)
import           Control.Lens
import           Control.Monad.Logger       (LoggingT, logDebugNS)
import           Control.Monad.Reader
import qualified Control.Monad.Trans        as Trans
import qualified Data.ByteString.Char8      as B8
import           Data.List                  (intersperse)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Formatting
import           Formatting.ShortFormatters
import qualified Path
import           Path.IO
import           System.Exit                (ExitCode (..))
import           System.Process             (ProcessHandle)
import qualified System.Process             as Process
import qualified System.Process.ByteString  as Proc.BS
import qualified System.Process.ListLike    as SPLL
import           System.Random              hiding (next)
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


sshEnv :: String -> String -> AbsFilePath -> SSHEnv
sshEnv user addr key =
  SSHEnv
  { _sshAddr        = addr
  , _sshPort        = 22
  , _sshKey         = key
  , _sshUser        = user
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
  -- ssh will fail with "too long for Unix domain socket" if the hostname
  -- is too long.
  -- Just use randint for the socket suffix if the controlpath is long
  let defaultControlPathFN =
        let prefix = ".ssh-"
            longSuffix =
              formatToString (s%":"%d%"-"%d)
                (env^.connectionString)
                (env^.sshPort)
                randint
            suffix =
              if length longSuffix <= 50
                then longSuffix
                else show randint
         in prefix++suffix
  defaultControlFP <- parseRelFile defaultControlPathFN
  let controlPath = fromMaybe defaultControlFP (env^.sshControlPath)
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
  let session =
        Session
        { _sessionEnv              = env
        , _sessionMasterProcess    = masterProc
        , _sessionMasterProcHandle = ph
        , _sessionControlPath      = controlPath
        , _sessionArgs             = args
        }
  testSession session
  return session


testSession :: Session -> IO ()
testSession session = do
  (exit', _, stderr') <- SPLL.readCreateProcessWithExitCode (sshProc session "exit") "" {- stdin -}
  case exit' of
    ExitSuccess -> return ()
    (ExitFailure code) -> do
      let env = session^.sessionEnv
      fail $
        formatToString
          (s%"@"%s%":"%d%" SSH connection failed with code "%d%": "%s)
          (env^.sshUser)
          (env^.sshAddr)
          (env^.sshPort)
          code
          stderr'


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
      \ce command args -> do
        let p = sshProc session $ sshExecStr ce command args
        logDebugNS "ssh" $ T.pack $ showProcess p
        runCreateProcess p
  , runExec_ =
      \ce command args -> do
        let p = sshProc session (sshExecStr ce command args)
        logDebugNS "ssh" $ T.pack $ showProcess p
        runCreateProcess_ (unwords (command:args)) p
  , runFileRead =
      \fp -> do
        let p = sshProc session $ sshExecStr craftEnvOverride "cat" [fromAbsFile fp]
        logDebugNS "ssh" $ T.pack $ showProcess p
        (ec, content, stderr') <-
          Trans.lift $ Proc.BS.readCreateProcessWithExitCode p ""
        unless (isSuccessCode ec) $
          $craftError $ "Failed to read file '"++ fromAbsFile fp ++"': " ++ B8.unpack stderr'
        return content
  , runFileWrite =
      \fp content -> do
        let p = sshProc session $ sshExecStr craftEnvOverride "tee" [fromAbsFile fp]
        logDebugNS "ssh" $ T.pack $ showProcess p
        (ec, _, stderr') <-
          Trans.lift $ Proc.BS.readCreateProcessWithExitCode p content
        unless (isSuccessCode ec) $
          $craftError $ "Failed to write file '" ++ fromAbsFile fp ++ "': " ++ B8.unpack stderr'
  , runSourceFile =
      \src dest -> do
        -- TODO: come up with a backup alternative if rsync is unavailable
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
    & craftExecEnvVars .~ Map.empty
    & craftCWD         .~ $(mkAbsDir "/")


runCraftSSH :: SSHEnv -> CraftEnv -> Craft a -> LoggingT IO a
runCraftSSH env ce configs =
  withSession env $ \session -> runCraft (runSSHSession session) ce configs


sshProc :: Session -> String
        -> Process.CreateProcess
sshProc session command = do
  Process.proc
    "ssh"
    $ (session^.sessionArgs)
      ++ (prependEachWith "-o"
          [ "ControlMaster=auto"
          , "ControlPersist=no"
          ])
      ++ [ session ^. sessionEnv . connectionString
         , command
         ]

sshExecStr :: CraftEnv -> Command -> Args -> String
sshExecStr ce command args = unwords (sudoArgs ++ ["sh", "-c", "'", shellStr, "'"])
 where
  sudoArgs :: [String]
  sudoArgs =
    let UserID uid' = ce^.craftUserID in
    ["sudo", "-u", "\\#"++(show uid'), "-n", "-H"]

  shellStr :: String
  shellStr = unwords (cdArgs ++ execEnvArgs ++ map escape (command:args))

  execEnvArgs :: [String]
  execEnvArgs = map escape . renderEnvVars $ ce ^. craftExecEnvVars

  cdArgs :: [String]
  cdArgs = ["cd", escape (fromAbsDir $ ce^.craftCWD), ";"]


specialChars :: [Char]
specialChars = " \\`!#$&,;'\"|{}()[]<>?"


escape :: String -> String
escape str = foldr (\ ch str' -> if ch `elem` specialChars then '\\':ch:str' else ch:str') [] str


renderEnvVars :: ExecEnvVars -> [String]
renderEnvVars = map (\(k, v) -> k++"="++v) . Map.toList
