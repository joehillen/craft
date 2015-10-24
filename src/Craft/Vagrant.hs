module Craft.Vagrant where

import Craft
import Craft.Ssh.Config
import qualified System.Environment
import qualified System.Directory
import Data.Maybe (fromMaybe)

runCraftVagrant :: PackageManager pm => CraftEnv pm -> Craft a -> IO a
runCraftVagrant env configs = do
  sysEnv <- System.Environment.getEnvironment
  cwd <- System.Directory.getCurrentDirectory
  sections <- runCraftLocal (craftEnv { craftExecEnv = sysEnv
                                      , craftExecCWD = cwd
                                      }) $
     parseExec parser stdout "vagrant" ["ssh-config"]

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
