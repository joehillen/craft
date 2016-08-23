module Craft.Run.Nspawn where

import           Control.Lens
import           Control.Monad.Logger (LoggingT)
import qualified Control.Monad.Trans as Trans
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           System.Process hiding ( readCreateProcessWithExitCode
                                       , readProcessWithExitCode)

import           Craft.Run.Internal
import           Craft.Types


runCraftNspawn :: Path Abs Dir -> CraftEnv -> Craft a -> LoggingT IO a
runCraftNspawn dir ce' = interpretCraft ce' run
 where
  run (Exec ce command args next) =
    let p = nspawnProc dir ce command args in execProc p next
  run (Exec_ ce command args next) =
    let p = nspawnProc dir ce command args in execProc_ (showProc p) p next
  run (FileRead _ fp next) = do
    fp' <- stripDir $(mkAbsDir "/") fp
    Trans.lift (BS.readFile $ fromAbsFile (dir</>fp')) >>= next
  run (FileWrite _ fp content next) = do
    fp' <- stripDir $(mkAbsDir "/") fp
    Trans.lift (BS.writeFile (fromAbsFile (dir</>fp')) content) >> next
  -- run (SourceFile ce src dest next) = do
  --   dest' <- stripDir $(mkAbsDir "/") dest
  --   run (Exec_ ce "/bin/cp" [fromAbsFile src, fromAbsFile (dir</>dest')] next)
  -- run (ReadSourceFile _ fp next) =
  --   Trans.lift (readSourceFileIO fp) >>= next
  -- run (FindSourceFile ce name next) =
  --   Trans.lift (findSourceFileIO ce name) >>= next


nspawnProc :: Path Abs Dir -> CraftEnv -> Command -> Args -> CreateProcess
nspawnProc dir ce cmd args =
  (proc "systemd-nspawn" ("-D":(fromAbsDir dir):"-q":cmd:args))
  { env           = Just $ Map.toList (ce ^. craftExecEnv)
  , cwd           = Just $ fromAbsDir (ce^.craftExecCWD)
  , close_fds     = True
  , create_group  = True
  , delegate_ctlc = False
  }
