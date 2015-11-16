module Craft.Run.Local where

import Control.Monad.Reader
import Control.Monad.Free
import qualified Data.ByteString as BS
import System.Process hiding ( readCreateProcessWithExitCode
                             , readProcessWithExitCode)

import Craft.Types
import Craft.Run.Internal


-- | runCraftLocal
runCraftLocal :: craftenv -> ReaderT craftenv (Free CraftDSL) a -> IO a
runCraftLocal e = iterM runCraftLocal' . flip runReaderT e


-- | runCraftLocal implementation
runCraftLocal' :: CraftDSL (IO a) -> IO a
runCraftLocal' (Exec cwd env command args next) = do
  let p = localProc cwd env command args
  execProc p next

runCraftLocal' (Exec_ cwd env command args next) = do
  let p = localProc cwd env command args
  execProc_ p next

runCraftLocal' (FileRead fp next) = do
  content <- BS.readFile fp
  next content

runCraftLocal' (FileWrite fp content next) = do
  BS.writeFile fp content
  next

runCraftLocal' (ReadSourceFile fps fp next) = do
  content <- readSourceFileIO fps fp
  next content


localProc :: CWD -> ExecEnv -> Command -> Args -> CreateProcess
localProc cwd env prog args =
  (proc prog args) { env           = Just env
                   , cwd           = Just cwd
                   , close_fds     = True
                   , create_group  = True
                   , delegate_ctlc = False
                   }
