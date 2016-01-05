module Craft.Run.Vagrant where

import Craft
import Craft.Config.Ssh

import Control.Lens
import qualified System.Environment
import qualified System.Directory
import Data.Maybe (fromMaybe)

runCraftVagrant :: CraftEnv -> Craft a -> IO a
runCraftVagrant env configs = do
  sysEnv <- System.Environment.getEnvironment
  cwd <- System.Directory.getCurrentDirectory
  sshcfg <- runCraftLocal (craftEnv (env ^. craftPackageManager)
                                    & craftExecEnv .~ sysEnv
                                    & craftExecCWD .~ cwd
                                    ) $ do
    r <- exec "vagrant" ["ssh-config"]
    success <- $errorOnFail r
    SshConfig <$> parseExecResult r parser (success ^. stdout)

  runCraftSSH
    (sshEnv (cfgLookupOrError "hostname" sshcfg)
            (cfgLookupOrError "identityfile" sshcfg)
            & sshUser .~ cfgLookupOrError "user" sshcfg
            & sshSudo .~ True)
    env
    configs

cfgLookupOrError :: Text -> SshConfig -> Text
cfgLookupOrError name sshcfg =
  fromMaybe
    (error $ "'" ++ name ++ "' not found in output of 'vagrant ssh-config'")
    (cfgLookup "default" name sshcfg)
