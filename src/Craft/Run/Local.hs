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
runCraftLocal' (Exec cwd env command args next) =
  let p = localProc cwd env command args in execProc p next
runCraftLocal' (Exec_ logger cwd env command args next) =
  let p = localProc cwd env command args in execProc_ logger p next
runCraftLocal' (FileRead fp next) = BS.readFile fp >>= next
runCraftLocal' (FileWrite fp content next) = BS.writeFile fp content >> next
runCraftLocal' (ReadSourceFile fps fp next) = readSourceFileIO fps fp >>= next
runCraftLocal' (Log action next) = action >> next


localProc :: CWD -> ExecEnv-> Command -> Args -> CreateProcess
localProc cwd env prog args =
  (proc prog args)
    { env           = Just env
    , cwd           = Just cwd
    , close_fds     = True
    , create_group  = True
    , delegate_ctlc = False
    }
