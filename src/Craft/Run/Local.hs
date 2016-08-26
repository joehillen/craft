module Craft.Run.Local where

import           Control.Lens
import           Control.Monad.Logger (LoggingT)
import qualified Control.Monad.Trans  as Trans
import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as Map
import           System.Process       hiding (readCreateProcessWithExitCode,
                                       readProcessWithExitCode)

import           Craft.Run.Internal
import           Craft.Types


-- | runCraftLocal
runCraftLocal :: CraftEnv -> Craft a -> LoggingT IO a
runCraftLocal ce' = interpretCraft ce' run
 where
  run (Exec ce command args next) =
    let p = localProc ce command args in execProc p next
  run (Exec_ ce command args next) =
    let p = localProc ce command args in execProc_ (showProc p) p next
  run (FileRead _ fp next) =
    Trans.lift (BS.readFile fp) >>= next
  run (FileWrite _ fp content next) =
    Trans.lift (BS.writeFile fp content) >> next
  run (SourceFile ce src dest next) =
    run (Exec_ ce "/bin/cp" [src, dest] next)
  run (ReadSourceFile _ fp next) =
    Trans.lift (readSourceFileIO fp) >>= next
  run (FindSourceFile ce name next) =
    Trans.lift (findSourceFileIO ce name) >>= next


localProc :: CraftEnv -> Command -> Args -> CreateProcess
localProc ce prog args =
  (proc prog args)
    { env           = Just $ Map.toList (ce ^. craftExecEnv)
    , cwd           = Just (ce ^. craftExecCWD)
    , close_fds     = True
    , create_group  = True
    , delegate_ctlc = False
    }
