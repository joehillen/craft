module Craft.Vagrant where

import Craft
import Craft.Ssh.Config
import System.Environment

runCraftVagrant :: PackageManager pm => CraftEnv pm -> Craft a -> IO a
runCraftVagrant env configs = do
  sysEnv <- getEnvironment
  sections <- runCraftLocal (craftEnv { craftExecEnv = sysEnv }) $
     parseExec parser "vagrant" ["ssh-config"]

  runCraftSSH
    (SSHEnv { sshEnvUser = cfgLookupOrError "user" sections
            , sshEnvAddr = cfgLookupOrError "hostname" sections
            , sshEnvKey  = cfgLookupOrError "identityfile" sections
            , sshSudo    = True
            })
    env
    configs

cfgLookupOrError :: String -> Sections -> String
cfgLookupOrError name sections =
  case cfgLookup "default" name sections of
    Nothing -> error $ "'" ++ name ++ "' not found in output of 'vagrant ssh-config'"
    Just x  -> x
