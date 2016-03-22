module Craft.Run.SSH where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Logger (LoggingT)
import qualified Control.Monad.Trans as Trans
import Control.Exception.Lifted (bracket)
import qualified Data.ByteString.Char8 as B8
import Data.List (intersperse)
import Data.String.Utils (replace)
import System.Directory
import qualified System.Process as Process
import System.Random hiding (next)
import qualified System.Process.ByteString as Proc.BS

import Craft.Types
import Craft.Helpers
import Craft.Run.Internal

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
  , _sshOptions = sshDefaultOptions
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


sshMasterProc :: SSHEnv -> SSHSession -> Process.CreateProcess
sshMasterProc sshenv session =
  (Process.proc "ssh" (sshArgs sshenv session
                      ++ [ "-o", "ControlMaster=yes"
                         , "-o", "ControlPersist=yes"
                         , connectionStr sshenv
                         ]))
    { Process.std_in  = Process.NoStream
    , Process.std_out = Process.NoStream
    , Process.std_err = Process.NoStream
    }


-- | runCraftSSH
runCraftSSH :: SSHEnv -> CraftEnv -> Craft a -> LoggingT IO a
runCraftSSH sshenv ce' prog = do
  session <- Trans.lift newSSHSession
  let controlpath = sshControlPath session
  bracket
    (do (_, _, _, ph) <- Trans.lift . Process.createProcess
                                    $ sshMasterProc sshenv session
        return ph)
    (\ph -> Trans.lift $ do
        Process.terminateProcess ph
        whenM (doesFileExist controlpath) $
          removeFile controlpath)
    (\_ -> interpretCraft ce' (run session) prog)
 where
  run session (Exec ce command args next) = do
    let p = sshProc sshenv session ce command args
    execProc p next
  run session (Exec_ ce command args next) = do
    let p = sshProc sshenv session ce command args
    execProc_ (unwords (command:args)) p next
  run session (FileRead ce fp next) = do
    let ceOverride = ce & craftExecEnv .~ []
                        & craftExecCWD .~ "/"
        p = sshProc sshenv session ceOverride "cat" [fp]
    (ec, content, stderr') <-
      Trans.lift $ Proc.BS.readCreateProcessWithExitCode p ""
    unless (isSuccess ec) $
      $craftError $ "Failed to read file '"++ fp ++"': " ++ B8.unpack stderr'
    next content
  run session (FileWrite ce fp content next) = do
    let ceOverride = ce & craftExecEnv .~ []
                        & craftExecCWD .~ "/"
        p = sshProc sshenv session ceOverride "tee" [fp]
    (ec, _, stderr') <-
      Trans.lift $ Proc.BS.readCreateProcessWithExitCode p content
    unless (isSuccess ec) $
      $craftError $ "Failed to write file '" ++ fp ++ "': " ++ B8.unpack stderr'
    next
  run session (SourceFile _ src dest next) = do
    let p = Process.proc "rsync"
              ([ "--quiet" -- suppress non-error messages
               , "--checksum" -- skip based on checksum, not mod-time & size
               , "--compress" -- compress file data during the transfer
                 -- specify the remote shell to use
               , "--rsh=ssh " ++ unwords (sshArgs sshenv session)
               ]
               ++ (if sshenv ^. sshSudo
                    then ["--super", "--rsync-path=sudo rsync"]
                    else [])
               ++ [ src
                  , connectionStr sshenv ++ ":" ++ dest
                  ])
    execProc_ (showProc p) p next
  run _ (FindSourceFile ce name next) =
    Trans.lift (findSourceFileIO ce name) >>= next
  run _ (ReadSourceFile _ fp next) =
    Trans.lift (readSourceFileIO fp) >>= next


sshArgs :: SSHEnv -> SSHSession -> Args
sshArgs SSHEnv{..} SSHSession{..} =
  [ "-p", show _sshPort ]
  ++ [ "-i", _sshKey ]
  ++ ["-o" | not (null _sshOptions)] ++ intersperse "-o" _sshOptions
  ++ [ "-o", "ControlPath=" ++ sshControlPath
     , "-o", "BatchMode=yes" -- never prompt for a password
     ]

connectionStr :: SSHEnv -> String
connectionStr SSHEnv{..} = concat [_sshUser, "@", _sshAddr]

sshProc :: SSHEnv -> SSHSession -> CraftEnv -> Command -> Args
        -> Process.CreateProcess
sshProc sshenv session ce command args =
  Process.proc "ssh" (sshArgs sshenv session
                      ++ [ "-o", "ControlMaster=auto"
                        , "-o", "ControlPersist=no"
                        , connectionStr sshenv
                        , cmd
                        ])
 where
  cwd = ce ^. craftExecCWD
  execEnv = ce ^. craftExecEnv
  cmd = unwords
        (sudo
         ++ ["sh", "-c", "'", "cd", escape cwd, ";"]
         ++ map escape (renderEnv execEnv)
         ++ (command : map escape args)
         ++ ["'"])
  sudo = if sshenv ^. sshSudo then ["sudo", "-H"] else []
  escape :: String -> String
  escape = recur backslash [" ", "*", "$", "'"]
  recur _ []     s = s
  recur f (a:as) s = recur f as $ f a s
  backslash x = replace x ('\\':x)


renderEnv :: ExecEnv -> [String]
renderEnv = map (\(k, v) -> k ++ "=" ++ v)
