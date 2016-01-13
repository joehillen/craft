module Craft.Run.Local where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Free
import Control.Monad.Except
import qualified Data.ByteString as BS
import System.Process hiding ( readCreateProcessWithExitCode
                             , readProcessWithExitCode)

import Craft.Types
import Craft.Run.Internal


-- | runCraftLocal
runCraftLocal :: CraftEnv -> Craft a -> IO a
runCraftLocal ce prog =
  (iterM runCraftLocal' . flip runReaderT ce $ runExceptT prog) >>= \case
    Left err -> error err
    Right a  -> return a


-- | runCraftLocal implementation
runCraftLocal' :: CraftDSL (IO a) -> IO a
runCraftLocal' (Exec ce command args next) =
  let p = localProc ce command args in execProc ce p next
runCraftLocal' (Exec_ ce command args next) =
  let p = localProc ce command args
  in execProc_ ce (showProc p) p next
runCraftLocal' (FileRead _ fp next) = BS.readFile fp >>= next
runCraftLocal' (FileWrite _ fp content next) = BS.writeFile fp content >> next
runCraftLocal' (SourceFile ce src dest next) = do
  runCraftLocal' (Exec_ ce "/bin/cp" [src, dest] next)
runCraftLocal' (ReadSourceFile _ fp next) = readSourceFileIO fp >>= next
runCraftLocal' (FindSourceFile ce name next) = findSourceFileIO ce name >>= next
runCraftLocal' (Log ce loc logsource level logstr next) =
  let logger = ce ^. craftLogger
  in logger loc logsource level logstr >> next


localProc :: CraftEnv -> Command -> Args -> CreateProcess
localProc ce prog args =
  (proc prog args)
    { env           = Just (ce ^. craftExecEnv)
    , cwd           = Just (ce ^. craftExecCWD)
    , close_fds     = True
    , create_group  = True
    , delegate_ctlc = False
    }
