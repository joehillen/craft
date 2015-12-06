module Craft.Run.Local where

import Control.Monad.Reader
import Control.Monad.Free
import qualified Data.ByteString as BS
import System.Process hiding ( readCreateProcessWithExitCode
                             , readProcessWithExitCode)

import Craft.Types
import Craft.Run.Internal


-- | runCraftLocal
runCraftLocal :: craftenv -> ReaderT craftenv (Free (CraftDSL pm)) a -> IO a
runCraftLocal e = iterM runCraftLocal' . flip runReaderT e


-- | runCraftLocal implementation
runCraftLocal' :: CraftDSL pm (IO a) -> IO a
runCraftLocal' (Exec env command args next) =
  let p = localProc env command args in execProc p next
runCraftLocal' (Exec_ env command args next) =
  let p = localProc env command args in execProc_ p next
runCraftLocal' (FileRead fp next) = BS.readFile fp >>= next
runCraftLocal' (FileWrite fp content next) = BS.writeFile fp content >> next
runCraftLocal' (ReadSourceFile fps fp next) = readSourceFileIO fps fp >>= next
runCraftLocal' (Log h bs next) = BS.hPutStr h bs >> next


localProc :: CraftEnv pm -> Command -> Args -> CreateProcess
localProc CraftEnv{..} prog args =
  (proc prog args)
    { env           = Just craftExecEnv
    , cwd           = Just craftExecCWD
    , close_fds     = True
    , create_group  = True
    , delegate_ctlc = False
    }
