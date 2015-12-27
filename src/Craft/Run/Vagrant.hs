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
  sections <- runCraftLocal (craftEnv (env ^. craftPackageManager)
                                      & craftExecEnv .~ sysEnv
                                      & craftExecCWD .~ cwd
                                      ) $ do
    r <- exec "vagrant" ["ssh-config"]
    success <- $errorOnFail r
    parseExecResult r parser $ success ^. stdout

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
