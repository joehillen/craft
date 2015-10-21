module Craft.Vagrant where

import Craft
import Craft.Ssh.Config
import System.Environment
import Data.Maybe (fromMaybe)

runCraftVagrant :: PackageManager pm => CraftEnv pm -> Craft a -> IO a
runCraftVagrant env configs = do
  sysEnv <- getEnvironment
  sections <- runCraftLocal (craftEnv { craftExecEnv = sysEnv }) $
     parseExec parser "vagrant" ["ssh-config"]

  runCraftSSH
    (sshEnv (cfgLookupOrError "hostname" sections)
            (cfgLookupOrError "identityfile" sections))
      { sshUser = cfgLookupOrError "user" sections
      , sshSudo    = True
      }
    env
    configs

cfgLookupOrError :: String -> Sections -> String
cfgLookupOrError name sections =
  fromMaybe
    (error $ "'" ++ name ++ "' not found in output of 'vagrant ssh-config'")
    (cfgLookup "default" name sections)
