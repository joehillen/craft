module Craft.Run.Nspawn where

import           Control.Lens
import qualified Control.Monad.Trans as Trans
import qualified Data.ByteString     as BS
import qualified Data.Map.Strict     as Map
import           System.Process      hiding (readCreateProcessWithExitCode,
                                      readProcessWithExitCode)

import           Craft.Run.Internal
import           Craft.Types


runNspawn :: AbsDirPath -> CraftRunner
runNspawn dir =
  CraftRunner
  { runExec =
      \ce command args ->
        let p = nspawnProc dir ce command args
        in runCreateProcess p
  , runExec_ =
      \ce command args ->
        let p = nspawnProc dir ce command args
        in runCreateProcess_ (showProcess p) p
  , runFileRead =
      \fp -> do
        fp' <- stripProperPrefix $(mkAbsDir "/") fp
        Trans.lift . BS.readFile . fromAbsFile $ dir</>fp'
  , runFileWrite =
      \fp content -> do
        fp' <- stripProperPrefix $(mkAbsDir "/") fp
        Trans.lift (BS.writeFile (fromAbsFile (dir</>fp')) content)
  , runSourceFile =
      \src dest -> do
        dest' <- stripProperPrefix $(mkAbsDir "/") dest
        let p = (proc "/bin/cp" [src, fromAbsFile (dir</>dest')])
                { env           = Nothing
                , cwd           = Nothing
                , close_fds     = True
                , create_group  = True
                , delegate_ctlc = False
                }
        runCreateProcess_ (showProcess p) p
  }


nspawnProc :: AbsDirPath -> CraftEnv -> Command -> Args -> CreateProcess
nspawnProc dir ce cmd args =
  let UserID uid' = ce^.craftUserID in
  (proc "systemd-nspawn" ("-D":(fromAbsDir dir):"-q":cmd:args))
  { env           = Just $ Map.toList (ce ^. craftExecEnvVars)
  , cwd           = Just $ fromAbsDir (ce^.craftCWD)
  , close_fds     = True
  , create_group  = True
  , delegate_ctlc = False
  , child_user    = Just $ fromIntegral uid'
  }
