module Craft.Run.Nspawn where

import           Control.Lens
import           Control.Monad.Logger (LoggingT)
import qualified Control.Monad.Trans  as Trans
import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as Map
import           System.Process       hiding (readCreateProcessWithExitCode,
                                       readProcessWithExitCode)

import           Craft.Run.Internal
import           Craft.Types


runNspawn :: FilePath -> CraftRunner
runNspawn dir =
  let dir' = reverse . dropWhile (== '/') $ reverse dir
  in CraftRunner
     { runExec =
         \ce command args ->
           let p = nspawnProc dir' ce command args
           in execProc p
     , runExec_ =
         \ce command args ->
           let p = nspawnProc dir' ce command args
           in execProc_ (showProc p) p
     , runFileRead =
         \fp -> Trans.lift . BS.readFile $ dir'++fp
     , runFileWrite =
         \fp content ->
           Trans.lift $ BS.writeFile (dir'++fp) content
     , runSourceFile =
         \src dest ->
           let p = (proc "/bin/cp" [src, (dir'++dest)])
                   { env           = Nothing
                   , cwd           = Nothing
                   , close_fds     = True
                   , create_group  = True
                   , delegate_ctlc = False
                   }
           in execProc_ (showProc p) p
     }


nspawnProc :: FilePath -> CraftEnv -> Command -> Args -> CreateProcess
nspawnProc dir ce cmd args =
  (proc "systemd-nspawn" ("-D":dir:"-q":cmd:args))
  { env           = Just $ Map.toList (ce ^. craftExecEnv)
  , cwd           = Just (ce ^. craftExecCWD)
  , close_fds     = True
  , create_group  = True
  , delegate_ctlc = False
  }
