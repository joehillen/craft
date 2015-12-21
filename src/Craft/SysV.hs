module Craft.SysV where

import Craft
import Control.Monad (when)
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


data Service = Service { name   :: String
                       , status :: Status
                       , atBoot :: Maybe Bool
                       }
  deriving (Eq, Show)


service :: String -> Service
service sn = Service { name = sn
                     , status = Running
                     , atBoot = Just True
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
    return . Just $ Service { name   = sn
                            , status = status'
                            , atBoot = Nothing
                            }
  else
    return Nothing


run :: String -> Service -> Craft ()
run cmd svc = exec_ (path (name svc)) [cmd]


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
  exec_ "/usr/sbin/update-rc.d" ["-f", name svc, cmd]


{-
instance Craftable Service where
  watchCraft svc = do
    get (name svc) >>= \case
      Nothing -> return (Unchanged, svc)
      Just a -> do
    when (status svc /= status r) $
      case status a of
        Running -> start a
        Stopped -> stop a
    case atBoot a of
      Nothing -> return ()
      Just x -> if x then updateRcD a "defaults"
                     else updateRcD a "remove"
-}
