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


runLocal :: CraftRunner
runLocal =
  CraftRunner
  { runExec =
      \ce command args ->
      let p = localProc ce command args
      in execProc p
  , runExec_ =
      \ce command args ->
      let p = localProc ce command args
      in execProc_ (showProc p) p
  , runFileRead =
      Trans.lift . BS.readFile
  , runFileWrite =
      \fp content ->
        Trans.lift (BS.writeFile fp content)
  , runSourceFile =
      \src dest ->
        let p = (proc "/bin/cp" [src, dest])
                { env           = Nothing
                , cwd           = Nothing
                , close_fds     = True
                , create_group  = True
                , delegate_ctlc = False
                }
        in execProc_ (showProc p) p
  }


localProc :: CraftEnv -> Command -> Args -> CreateProcess
localProc ce prog args =
  (proc prog args)
    { env           = Just $ Map.toList (ce ^. craftExecEnv)
    , cwd           = Just (ce ^. craftExecCWD)
    , close_fds     = True
    , create_group  = True
    , delegate_ctlc = False
    }
