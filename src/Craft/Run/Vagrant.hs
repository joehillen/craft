module Craft.Run.Vagrant where

import Craft
import Craft.Config.Ssh (SshConfig(..), parser, cfgLookup)

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

  let addr = cfgLookupOrError "hostname" sshcfg
      port = read $ cfgLookupOrError "port" sshcfg
      user = cfgLookupOrError "user" sshcfg
      key  = cfgLookupOrError "identityfile" sshcfg
  runCraftSSH (sshEnv addr key & sshUser .~ user
                               & sshPort .~ port
                               & sshSudo .~ True)
              env
              configs

cfgLookupOrError :: String -> SshConfig -> String
cfgLookupOrError name sshcfg =
  fromMaybe
    (error $ "'" ++ name ++ "' not found in output of 'vagrant ssh-config'")
    (cfgLookup "default" name sshcfg)
