module Craft.Run.Local where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Free
import qualified Data.ByteString as BS
import System.Process hiding ( readCreateProcessWithExitCode
                             , readProcessWithExitCode)

import Craft.Types
import Craft.Run.Internal


-- | runCraftLocal
runCraftLocal :: CraftEnv pm -> ReaderT (CraftEnv pm) (Free (CraftDSL pm)) a -> IO a
runCraftLocal ce = iterM runCraftLocal' . flip runReaderT ce


-- | runCraftLocal implementation
runCraftLocal' :: CraftDSL pm (IO a) -> IO a
runCraftLocal' (Exec ce command args next) =
  let p = localProc ce command args in execProc ce p next
runCraftLocal' (Exec_ ce command args next) =
  let p = localProc ce command args
  in execProc_ ce (showProc p) p next
runCraftLocal' (FileRead _ fp next) = BS.readFile fp >>= next
runCraftLocal' (FileWrite _ fp content next) = BS.writeFile fp content >> next
runCraftLocal' (SourceFile ce src dest next) = do
  src' <- findSourceFile ce src
  runCraftLocal' (Exec_ ce "/bin/cp" [src', dest] next)
runCraftLocal' (ReadSourceFile ce fp next) = readSourceFileIO ce fp >>= next
runCraftLocal' (Log ce loc logsource level logstr next) =
  let logger = ce ^. craftLogger
  in logger loc logsource level logstr >> next


localProc :: CraftEnv pm -> Command -> Args -> CreateProcess
localProc ce prog args =
  (proc prog args)
    { env           = Just (ce ^. craftExecEnv)
    , cwd           = Just (ce ^. craftExecCWD)
    , close_fds     = True
    , create_group  = True
    , delegate_ctlc = False
    }
