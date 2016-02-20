{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Craft.Systemd where

import Control.Lens
import Data.List.Utils (join)

import Craft
import Craft.Directory
import           Craft.File (File, file)
import qualified Craft.File as File
import           Craft.File.Mode

-- Helpful: https://www.digitalocean.com/community/tutorials/understanding-systemd-units-and-unit-files

-- A service name is simply the name of that service
type ServiceName = String

-- All systemd Units can be transformed to their plaintext output
class WritableUnit a where
  transformUnit :: a -> String

-- A generic implementation for the String type
instance WritableUnit String where
  transformUnit sn = show sn

-- A generic implementation for the Bool type
instance WritableUnit Bool where
  transformUnit True = "true"
  transformUnit False = "false"

-- A generic implementation for the Int type
instance WritableUnit Int where
  transformUnit i = show i

-- Composite units need to be able to furnish a filename (*without* a path)
class CompositeUnit unit where
  getFileName :: unit -> FilePath

-- | Helpers |--
-- =====================================================================
-- Functions for extracting optional values to be transformed
writeOptionalSection :: WritableUnit a => Maybe a -> String
writeOptionalSection unit =
  case unit of
    {
      Nothing -> "";
      Just u -> transformUnit u ++ "\n"
    }

-- Optionally write a directive
writeOptional :: WritableUnit a => String -> Maybe a -> String
writeOptional directive optionalParam =
  case optionalParam of
    {
      Nothing -> "";
      Just param -> directive ++ "=" ++ transformUnit param ++ "\n"
    }

writeOptionalList :: WritableUnit a => String -> Maybe [a] -> String
writeOptionalList directive optionalParams =
  case optionalParams of
    {
      Nothing -> "";
      Just params -> directive ++ "=" ++
      (join " " (map transformUnit params)) ++ "\n"
    }
-- =====================================================================


-- | [Unit] |--
-- =====================================================================
data UnitSection = UnitSection { _description   :: String
                               , _documentation :: Maybe String
                               , _requires      :: Maybe [ServiceName]
                               , _wants         :: Maybe [ServiceName]
                               , _bindsTo       :: Maybe [ServiceName]
                               , _before        :: Maybe [ServiceName]
                               , _after         :: Maybe [ServiceName]
                               , _conflicts     :: Maybe [ServiceName]
                               , _condition     :: Maybe String
                               , _assert        :: Maybe String
                               } deriving (Eq, Show)
unitSection :: UnitSection
unitSection = UnitSection { _description = undefined
                          , _documentation = Nothing
                          , _requires = Nothing
                          , _wants = Nothing
                          , _bindsTo = Nothing
                          , _before = Nothing
                          , _after = Nothing
                          , _conflicts = Nothing
                          , _condition = Nothing
                          , _assert = Nothing }
makeLenses ''UnitSection

instance WritableUnit UnitSection where
  transformUnit u =
    "[Unit]\n" ++
    "Description=" ++ u^.description ++ "\n"
    ++ writeOptional "Documentation" (_documentation u)
    ++ writeOptionalList "Requires" (_requires u)
    ++ writeOptionalList "Wants" (_wants u)
    ++ writeOptionalList "BindsTo" (_bindsTo u)
    ++ writeOptionalList "Before" (_before u)
    ++ writeOptionalList "After" (_after u)
    ++ writeOptionalList "Conflicts" (_conflicts u)
    ++ writeOptional "Condition" (_condition u)
    ++ writeOptional "Assert" (_assert u)
-- =====================================================================


-- | [Install] | --
-- =====================================================================
data InstallSection = InstallSection { _wantedBy :: Maybe String
                                     , _requiredBy :: Maybe String
                                     , _alias :: Maybe String
                                     , _also :: Maybe String
                                     , _defaultInstance :: Maybe String
                                     } deriving (Eq, Show)

installSection :: InstallSection
installSection = InstallSection { _wantedBy = Nothing
                                , _requiredBy = Nothing
                                , _alias = Nothing
                                , _also = Nothing
                                , _defaultInstance = Nothing}

makeLenses ''InstallSection

instance WritableUnit InstallSection where
  transformUnit u =
    "[Install]\n"
    ++ writeOptional "WantedBy" (_wantedBy u)
    ++ writeOptional "RequiredBy" (_requiredBy u)
    ++ writeOptional "Alias" (_alias u)
    ++ writeOptional "Also" (_also u)
    ++ writeOptional "DefaultInstance" (_defaultInstance u)
-- =====================================================================


-- | Service | --
-- =====================================================================
data ServiceType = Simple
                 | Forking
                 | Oneshot
                 | Dbus
                 | Notify
                 | Idle
                 deriving (Eq, Show)

instance WritableUnit ServiceType where
  transformUnit Simple  = "simple"
  transformUnit Forking = "forking"
  transformUnit Oneshot = "oneshot"
  transformUnit Dbus    = "dbus"
  transformUnit Notify  = "notify"
  transformUnit Idle    = "idle"

-- How service restarts should be handled
data ServiceRestart = RestartAlways
                    | RestartOnSuccess
                    | RestartOnFailure
                    | RestartOnAbnormal
                    | RestartOnAbort
                    | RestartOnWatcher
                    deriving (Eq, Show)

instance WritableUnit ServiceRestart where
  transformUnit RestartAlways    = "always"
  transformUnit RestartOnSuccess = "on-success"
  transformUnit RestartOnFailure = "on-failure"
  transformUnit RestartOnAbnormal = "on-abnormal"
  transformUnit RestartOnAbort = "on-abort"
  transformUnit RestartOnWatcher = "on-watchdog"

data ServiceNotifyAccess = NotifyNone
                         | NotifyMain
                         | NotifyAll
                         deriving (Eq, Show)

instance WritableUnit ServiceNotifyAccess where
  transformUnit NotifyNone = "none"
  transformUnit NotifyMain = "main"
  transformUnit NotifyAll  = "all"

-- A "Service" declaration
data ServiceSection = ServiceSection {
  _serviceType       :: ServiceType
  , _execStart       :: Maybe String
  , _execStartPre    :: Maybe String
  , _execStartPost   :: Maybe String
  , _execReload      :: Maybe String
  , _execStop        :: Maybe String
  , _execStopPost    :: Maybe String
                        -- TODO use actual 'Env' here
  , _environment     :: Maybe String
  , _restartSec      :: Maybe Int
  , _restartService  :: Maybe ServiceRestart
  , _remainAfterExit :: Maybe Bool
  , _pidFile         :: Maybe FilePath
  , _busName         :: Maybe String
  , _notifyAccess    :: Maybe ServiceNotifyAccess
  } deriving (Eq, Show)

serviceSection :: ServiceSection
serviceSection = ServiceSection { _serviceType = Simple
                                , _execStart = Nothing
                                , _execStartPre = Nothing
                                , _execStartPost = Nothing
                                , _execReload = Nothing
                                , _execStop = Nothing
                                , _execStopPost = Nothing
                                , _environment = Nothing
                                , _restartSec = Nothing
                                , _restartService = Just RestartAlways
                                , _remainAfterExit = Nothing
                                , _pidFile = Nothing
                                , _busName = Nothing
                                , _notifyAccess = Nothing
                                }
makeLenses ''ServiceSection

instance WritableUnit ServiceSection where
  transformUnit u =
    "[Service]\n" ++
    "Type=" ++ (transformUnit (_serviceType u)) ++ "\n"
    ++ writeOptional "ExecStart" (_execStart u)
    ++ writeOptional "ExecStartPre" (_execStartPre u)
    ++ writeOptional "ExecStartPost" (_execStartPost u)
    ++ writeOptional "ExecReload" (_execReload u)
    ++ writeOptional "ExecStop" (_execStop u)
    ++ writeOptional "ExecStopPost" (_execStopPost u)
    ++ writeOptional "Environment" (_environment u)
    ++ writeOptional "RestartSec" (_restartSec u)
    ++ writeOptional "Restart" (_restartService u)
    ++ writeOptional "RemainAfterExit" (_remainAfterExit u)
    ++ writeOptional "PIDFile" (_pidFile u)
    ++ writeOptional "BusName" (_busName u)
    ++ writeOptional "NotifyAccess" (_notifyAccess u)
-- =====================================================================


-- | [Mount] | --
-- =====================================================================
data MountSection = MountSection {
  _mountWhat :: String
  , _mountWhere :: String
  , _mountType :: String
  , _mountOptions :: Maybe String
  , _mountSloppyOptions :: Maybe Bool
  , _directoryMode :: Maybe String
  , _timeoutSec :: Maybe Int
  } deriving (Eq, Show)

mountSection :: MountSection
mountSection = MountSection { _mountWhat = undefined
                            , _mountWhere = undefined
                            , _mountType = undefined
                            , _mountOptions = Nothing
                            , _mountSloppyOptions = Nothing
                            , _directoryMode = Nothing
                            , _timeoutSec = Nothing}

makeLenses ''MountSection

instance WritableUnit MountSection where
  transformUnit u =
    "[Mount]\n"
    ++ writeOptional "What" (Just (_mountWhat u))
    ++ writeOptional "Where" (Just (_mountWhere u))
    ++ writeOptional "Type" (Just (_mountType u))
    ++ writeOptional "Options" (_mountOptions u)
    ++ writeOptional "SloppyOptions" (_mountSloppyOptions u)
    ++ writeOptional "DirectoryMode" (_directoryMode u)
    ++ writeOptional "TimeoutSec" (_timeoutSec u)

-- =====================================================================

---- Systemd sections not implemented yet (TODO):
data SocketSection = SocketSection {} deriving (Eq, Show)
data DeviceSection = DeviceSection {} deriving (Eq, Show)
data AutomountSection = AutomountSection {} deriving (Eq, Show)
data SwapSection = SwapSection {} deriving (Eq, Show)
data PathSection = PathSection {} deriving (Eq, Show)
data TargetSection = TargetSection {} deriving (Eq, Show)
data TimerSection = TimerSection {} deriving (Eq, Show)
data SnapshotSection = SnapshotSection {} deriving (Eq, Show)
data SliceSection = SliceSection {} deriving (Eq, Show)
data ScopeSection = ScopeSection {} deriving (Eq, Show)
----


-- | Top-level Systemd units | --
-- =====================================================================
data SystemdUnit a = SystemdUnit
                     { _name         :: String
                     , _unit         :: Maybe UnitSection
                     , _mainSection  :: a
                     , _install      :: Maybe InstallSection }
                   deriving (Eq, Show)

type Service = SystemdUnit ServiceSection
type Mount   = SystemdUnit MountSection

instance WritableUnit Service where
  transformUnit service =
    writeOptionalSection (_unit service)
    ++ transformUnit (_mainSection service) ++ "\n"
    ++ writeOptionalSection (_install service)

instance WritableUnit Mount where
  transformUnit mount =
    writeOptionalSection (_unit mount)
    ++ transformUnit (_mainSection mount) ++ "\n"
    ++ writeOptionalSection (_install mount)

-- Location *.service, etc files will be installed
systemdUnitLocation :: FilePath
systemdUnitLocation = "/etc/systemd/system/"

systemdUnitDir :: Directory
systemdUnitDir = Directory systemdUnitLocation (Mode RWX O O) 0 0

instance Craftable Service where
  watchCraft svc = do
    craft_ $ systemdUnitDir
    w <- watchCraft_ $ file (systemdUnitLocation </> ((_name svc) ++ ".service"))
      & File.mode .~ Mode RW O O
      & File.ownerID .~ 0
      & File.groupID .~ 0
      & File.strContent .~ transformUnit svc
    when (changed w) daemonReload
    return (w, svc)

instance Craftable Mount where
  watchCraft mount = do
    craft_ $ systemdUnitDir
    w <- watchCraft_ $ file (systemdUnitLocation </> ((_name mount) ++ ".mount"))
      & File.mode .~ Mode RW O O
      & File.ownerID .~ 0
      & File.groupID .~ 0
      & File.strContent .~ transformUnit mount
    when (changed w) daemonReload
    return (w, mount)

-- =====================================================================



---- TODO Functions to deal with systemd
systemdBin :: FilePath
systemdBin = "/usr/bin/systemd"

systemdCtlBin :: FilePath
systemdCtlBin = "/usr/bin/systemctl"

-- Return the absolute path a service should be written to
fileForUnit :: CompositeUnit unit => unit -> File
fileForUnit unit = (file $ systemdUnitLocation </> getFileName unit)

daemonReload :: Craft()
daemonReload = exec_ systemdCtlBin ["daemon-reload"]

start :: ServiceName -> Craft ()
start service = exec_ systemdBin ["start", service]

stop :: ServiceName -> Craft ()
stop service = exec_ systemdBin ["stop", service]

restart :: ServiceName -> Craft ()
restart service = exec_ systemdBin ["restart", service]

-- TODO: need to return 'true' for running and 'false' for not running here
status :: ServiceName -> Craft ()
status service = exec_ systemdBin ["status", service]


---- Examples
-- This is an example service definition, that looks like this (in the file):
--
-- [Unit]
-- Description=Redshift
--
-- [Service]
-- Type=simple
-- ExecStart=/usr/bin/redshift -l geoclue2 -t 6500:3700
-- ExecStop=/usr/bin/pkill redshift
-- Environment=DISPLAY=:0
-- Restart=always
--
-- [Install]
-- WantedBy=default.target
--
redshift :: Service
redshift = SystemdUnit {
  _name = "redshift"
  , _unit = Just unitSection { _description = "Redshift" }
  , _mainSection = serviceSection {
      _serviceType = Simple
      , _execStart = Just "/usr/bin/redshift -l geoclue2 -t 6500:3700"
      , _execStop = Just "/usr/bin/pkill redshift"
      , _environment = Just "DISPLAY=:0"
      , _restartService = Just RestartAlways
      }
  , _install = Just installSection { _wantedBy = Just "default.target" }
  }

-- You should then be able to run:
-- putStrLn $ transformUnit redshift
