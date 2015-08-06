-- | Simple example of remote execution

module Craft.Exec.Ssh where

import           Data.ByteString (ByteString)
import qualified System.Process.ByteString as PBS

import           Craft


data SshAuth
  = SshIndentity FilePath
  {- SshPassword String -}

data SshExecuter
  = SshExecuter
    { sshExecHost    :: String
    , sshExecUser    :: String
    , sshExecPort    :: Int
    , sshExecAuth    :: SshAuth
    , sshExecOptions :: [(String, String)]
    }

sshExecuter :: String -> SshAuth -> SshExecuter
sshExecuter host auth =
  SshExecuter
  { sshExecHost = host
  , sshExecAuth = auth
  , sshExecPort = 22
  , sshExecUser = "root"
  , sshExecOptions = [ ("StrictHostKeyChecking", "no")
                     , ("UserKnownHostsFile", "/dev/null")
                     ]
  }

instance Executer SshExecuter where
  executer   = sshExec
  executer_  = sshExec_
  fileReader ex fp   = do
    (ec, stdout, stderr) <- sshExecBS ex "" "cat" [fp]
    return stdout
  fileWriter ex fp c = do
    (ec, _stdout, stderr) <- sshExecBS ex c "tee" [fp]
    return ()

{- This is not implemented because it does not support withCWD or withPath.
 - I've got an idea on how to do it, but it will take more work than
 - it's worth right now. :(
 -}
sshExec :: SshExecuter -> Command -> Args -> Craft (ExitCode, StdOut, StdErr)
sshExec ex prog args = notImplemented "Exec.Ssh.sshExec_"
  -- localExec "ssh" $ mkSshArgs ex prog args

sshExec_ :: SshExecuter -> Command -> Args -> Craft ()
sshExec_ ex prog args = notImplemented "Exec.Ssh.sshExec_"
  -- localExec_ "ssh" $ mkSshArgs ex prog args

sshExecBS :: SshExecuter
          -> ByteString -- stdin
          -> Command
          -> Args
          -> Craft (ExitCode, ByteString, ByteString)
sshExecBS ex stdin prog args = do
  p <- craftProc "ssh" $ mkSshArgs ex prog args
  liftIO $ PBS.readCreateProcessWithExitCode p stdin


mkSshArgs :: SshExecuter -> Command -> Args -> Args
mkSshArgs SshExecuter{..} prog args =
  "-q":opts
  ++ auth sshExecAuth
  ++ ["-p", show sshExecPort]
  ++ [sshExecUser ++ "@" ++ sshExecHost]
  ++ [unwords $ prog:args]
 where
  opts = concatMap (\(n,v) -> ["-o", n ++ "=" ++ v]) sshExecOptions
  auth (SshIndentity fp) = ["-i", fp]

data SshSudoExecuter
  = SshSudoExecuter
    { sshSudoExecUser     :: String
    , sshSudoExecPassword :: Maybe String
    , sshSudoExecSshExec  :: SshExecuter
    }

sshSudoExecuter :: SshExecuter -> SshSudoExecuter
sshSudoExecuter sshEx =
  SshSudoExecuter
  { sshSudoExecUser = "root"
  , sshSudoExecPassword = Nothing
  , sshSudoExecSshExec  = sshEx
  }

instance Executer SshSudoExecuter where
  executer   ex prog args = sshExec  (sshSudoExecSshExec ex) "sudo" (prog:args)
  executer_  ex prog args = sshExec_ (sshSudoExecSshExec ex) "sudo" (prog:args)
  fileReader ex fp   = do
    (ec, stdout, stderr) <- sshExecBS (sshSudoExecSshExec ex) "" "sudo" ["cat",fp]
    return stdout
  fileWriter ex fp c = do
    (ec, _stdout, stderr) <- sshExecBS (sshSudoExecSshExec ex) c "sudo" ["tee",fp]
    return ()
