module Craft.SysV where

import Control.Lens

import Craft
import qualified Craft.File as File


data Status = Running
            | Stopped
  deriving (Eq, Show)


data RunLevel = RunLevel0
              | RunLevel1
              | RunLevel2
              | RunLevel3
              | Runlevel4
              | Runlevel5
              | Runlevel6
              | RunlevelS
  deriving (Eq)


instance Show RunLevel where
  show RunLevel0 = "0"
  show RunLevel1 = "1"
  show RunLevel2 = "2"
  show RunLevel3 = "3"
  show Runlevel4 = "4"
  show Runlevel5 = "5"
  show Runlevel6 = "6"
  show RunlevelS = "S"


defaults :: [RunLevel]
defaults = [ RunLevel0
           , RunLevel1
           , RunLevel2
           , RunLevel3
           , Runlevel4
           , Runlevel5
           , Runlevel6
           ]


data Service = Service { _name   :: String
                       , _status :: Status
                       , _atBoot :: Maybe Bool
                       }
  deriving (Eq, Show)
makeLenses ''Service


service :: String -> Service
service sn = Service { _name = sn
                     , _status = Running
                     , _atBoot = Just True
                     }


getStatus :: String -> Craft Status
getStatus sn =
  exec (path sn) ["status"] >>= \case
    (ExecFail _) -> return Stopped
    (ExecSucc _) -> return Running


path :: String -> FilePath
path sn = "/etc/init.d" </> sn


get :: String -> Craft (Maybe Service)
get sn = do
  exists <- File.exists (path sn)
  if exists then do
    status' <- getStatus sn
    return . Just $ Service { _name   = sn
                            , _status = status'
                            , _atBoot = Nothing
                            }
  else
    return Nothing


run :: String -> Service -> Craft ()
run cmd svc = exec_ (path (svc ^. name)) [cmd]


start :: Service -> Craft ()
start = run "start"


stop :: Service -> Craft ()
stop = run "stop"


restart :: Service -> Craft ()
restart = run "restart"


reload :: Service -> Craft ()
reload = run "reload"


updateRcD :: Service -> String -> Craft ()
updateRcD svc cmd =
  exec_ "/usr/sbin/update-rc.d" ["-f", svc ^. name, cmd]


instance Craftable Service where
  watchCraft svc = do
    let sn = svc ^. name
    whenJust (svc ^. atBoot) $ \x ->
      updateRcD svc (if x then "defaults" else "remove")

    status' <- getStatus sn
    case (svc ^. status, status') of
      (Running, Running) -> return (Unchanged, svc)
      (Stopped, Stopped) -> return (Unchanged, svc)
      (Running, Stopped) -> do
        start svc
        return (Updated, svc)
      (Stopped, Running) -> do
        stop svc
        return (Updated, svc)
