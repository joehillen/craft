module Craft.Run.Nspawn where

import           Control.Lens
import           Control.Monad.Logger (LoggingT)
import qualified Control.Monad.Trans as Trans
import qualified Data.ByteString as BS
import           System.Process hiding ( readCreateProcessWithExitCode
                                       , readProcessWithExitCode)

import           Craft.Run.Internal
import           Craft.Types


runCraftNspawn :: FilePath -> CraftEnv -> Craft a -> LoggingT IO a
runCraftNspawn dir ce' = interpretCraft ce' run
 where
  dir' = reverse . dropWhile (== '/') $ reverse dir
  run (Exec ce command args next) =
    let p = nspawnProc dir ce command args in execProc p next
  run (Exec_ ce command args next) =
    let p = nspawnProc dir ce command args in execProc_ (showProc p) p next
  run (FileRead _ fp next) =
    Trans.lift (BS.readFile (dir'++fp)) >>= next
  run (FileWrite _ fp content next) =
    Trans.lift (BS.writeFile (dir'++fp) content) >> next
  run (SourceFile ce src dest next) =
    run (Exec_ ce "/bin/cp" [src, (dir'++dest)] next)
  run (ReadSourceFile _ fp next) =
    Trans.lift (readSourceFileIO fp) >>= next
  run (FindSourceFile ce name next) =
    Trans.lift (findSourceFileIO ce name) >>= next


nspawnProc :: FilePath -> CraftEnv -> Command -> Args -> CreateProcess
nspawnProc dir ce cmd args =
  (proc "systemd-nspawn" ("-D":dir:"-q":cmd:args))
  { env           = Just (ce ^. craftExecEnv)
  , cwd           = Just (ce ^. craftExecCWD)
  , close_fds     = True
  , create_group  = True
  , delegate_ctlc = False
  }
