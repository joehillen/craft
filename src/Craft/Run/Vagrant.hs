module Craft.Run.Vagrant where

import           Control.Lens
import           Control.Monad.Logger (LoggingT)
import qualified Control.Monad.Trans as Trans
import           Data.Maybe (fromMaybe, maybeToList)
import qualified System.Directory
import qualified System.Environment

import           Craft
import           Craft.Config.SSH (SSHConfig(..), parser, cfgLookup)


data VagrantSettings
  = VagrantSettings
    { vagrantUp  :: Bool
    , vagrantBox :: String
    }


vagrantSettings :: VagrantSettings
vagrantSettings =
  VagrantSettings
  { vagrantUp = False
  , vagrantBox = "default"
  }


runCraftVagrant :: VagrantSettings -> CraftEnv -> Craft a -> LoggingT IO a
runCraftVagrant settings env configs = do
  sysEnv <- Trans.lift System.Environment.getEnvironment
  cwd <- Trans.lift System.Directory.getCurrentDirectory
  let box = vagrantBox settings
  sshcfg <- runCraftLocal (craftEnv (env ^. craftPackageManager)
                                    & craftExecEnv .~ sysEnv
                                    & craftExecCWD .~ cwd
                                    ) $ do
    when (vagrantUp settings) $ exec_ "vagrant" ["up", box]
    SSHConfig <$> parseExecStdout parser "vagrant" ["ssh-config", box]

  let addr = cfgLookupOrError box "hostname" sshcfg
      port = read $ cfgLookupOrError box "port" sshcfg
      user = cfgLookupOrError box "user" sshcfg
      key  = cfgLookupOrError box "identityfile" sshcfg
  runCraftSSH (sshEnv addr key & sshUser .~ user
                               & sshPort .~ port
                               & sshSudo .~ True)
              env
              configs


cfgLookupOrError :: String -> String -> SSHConfig -> String
cfgLookupOrError box name sshcfg =
  fromMaybe
    (error $ "'"++name++"' not found in output of 'vagrant ssh-config "++box++"'")
    (cfgLookup box name sshcfg)
