module Craft.SysV where

import           Control.Lens

import           Craft
import qualified Craft.File   as File


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


data Service = Service
  { _serviceName   :: String
  , _serviceStatus :: Status
  , _serviceAtBoot :: Maybe Bool
  }
  deriving (Eq, Show)
makeLenses ''Service


service :: String -> Service
service sn =
  Service
  { _serviceName   = sn
  , _serviceStatus = Running
  , _serviceAtBoot = Just True
  }


servicePath :: String -> Craft AbsFilePath
servicePath sn = do
  fn <- parseRelFile sn
  return $ $(mkAbsDir "/etc/init.d")</>fn


get :: String -> Craft (Maybe Service)
get sn = do
  sp <- servicePath sn
  exists <- File.exists sp
  if exists then do
    status' <- exec (fromAbsFile sp) ["status"]>>= \case
                 (Failure _) -> return Stopped
                 (Success _) -> return Running
    return . Just $ Service { _serviceName   = sn
                            , _serviceStatus = status'
                            , _serviceAtBoot = Nothing
                            }
  else
    return Nothing


status :: Service -> Craft Status
status svc = do
  sp <- servicePath $ svc^.serviceName
  exec (fromAbsFile sp) ["status"] >>= \case
    (Failure _) -> return Stopped
    (Success _) -> return Running


run_ :: String -> Service -> Craft ()
run_ cmd svc = do
  sp <- servicePath $ svc^.serviceName
  exec_ (fromAbsFile sp) [cmd]


start :: Service -> Craft ()
start = run_ "start"


stop :: Service -> Craft ()
stop = run_ "stop"


restart :: Service -> Craft ()
restart = run_ "restart"


reload :: Service -> Craft ()
reload = run_ "reload"


updateRcD :: Service -> String -> Craft ()
updateRcD svc cmd =
  exec_ "update-rc.d" ["-f", svc^.serviceName, cmd]


instance Craftable Service Service where
  watchCraft svc = do
    whenJust (svc^.serviceAtBoot) $ \x ->
      updateRcD svc (if x then "defaults" else "remove")
    status' <- status svc
    case (svc^.serviceStatus, status') of
      (Running, Running) -> return (Unchanged, svc)
      (Stopped, Stopped) -> return (Unchanged, svc)
      (Running, Stopped) -> do
        start svc
        return (Updated, svc)
      (Stopped, Running) -> do
        stop svc
        return (Updated, svc)
