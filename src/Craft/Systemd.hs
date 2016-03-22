{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Craft.Systemd where

import           Control.Lens
import           Data.List.Utils (join)
import           Data.Char as Char

import           Craft
import qualified Craft.Directory as Dir
import           Craft.File (file)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.User

-- Helpful: https://www.digitalocean.com/community/tutorials/understanding-systemd-units-and-unit-files

-- A service name is simply the name of that service
type ServiceName = String

-- All systemd Units can be transformed to their plaintext output
class WritableUnit a where
  transformUnit :: a -> String

-- A generic implementation for the String type
instance WritableUnit String where
  transformUnit sn = sn

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
      (join " " (Prelude.map transformUnit params)) ++ "\n"
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
unitSection :: String -> UnitSection
unitSection s = UnitSection { _description = s
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
                                , _defaultInstance = Nothing
                                }

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


-- | Execution Environment | --
-- =====================================================================
data IOSchedulingClass = IONone | IORealtime | IOBestEffort | IOIdle
                       deriving (Eq, Show)

instance WritableUnit IOSchedulingClass where
  transformUnit IONone = "none"
  transformUnit IORealtime = "realtime"
  transformUnit IOBestEffort = "best-effort"
  transformUnit IOIdle = "idle"

data CPUSchedulingPolicy = CPUOther | CPUBatch | CPUIdle | CPUFifo | CPURr
                         deriving (Eq, Show)

instance WritableUnit CPUSchedulingPolicy where
  transformUnit CPUOther = "other"
  transformUnit CPUBatch = "batch"
  transformUnit CPUIdle = "idle"
  transformUnit CPUFifo = "fifo"
  transformUnit CPURr = "rr"

data StandardInput = SINull | SITty | SITtyForce | SITtyFail | SISocket
                   deriving (Eq, Show)

instance WritableUnit StandardInput where
  transformUnit SINull = "null"
  transformUnit SITty = "tty"
  transformUnit SITtyForce = "tty-force"
  transformUnit SITtyFail = "tty-fail"
  transformUnit SISocket = "socket"

data StandardOutput = SOInherit
                    | SONull
                    | SOTty
                    | SOJournal
                    | SOSyslog
                    | SOKmsg
                    | SOJournalAndConsole
                    | SOSyslogAndConsole
                    | SOKmsgAndConsole
                    | SOSocket
                    deriving (Eq, Show)

instance WritableUnit StandardOutput where
  transformUnit SOInherit = "inherit"
  transformUnit SONull = "null"
  transformUnit SOTty = "tty"
  transformUnit SOJournal = "journal"
  transformUnit SOSyslog = "syslog"
  transformUnit SOKmsg = "kmsg"
  transformUnit SOJournalAndConsole = "journal+console"
  transformUnit SOSyslogAndConsole = "syslog+console"
  transformUnit SOKmsgAndConsole = "kmsg+console"
  transformUnit SOSocket = "socket"

data SyslogFacility = Kern
                    | User
                    | Mail
                    | Daemon
                    | Auth
                    | Syslog
                    | Lpr
                    | News
                    | Uucp
                    | Cron
                    | Authpriv
                    | Ftp
                    | Local0
                    | Local1
                    | Local2
                    | Local3
                    | Local4
                    | Local5
                    | Local6
                    | Local7
                    deriving (Eq, Show)

-- Lowercase the first l
lowerStr :: String -> String
lowerStr "" = ""
lowerStr (s:xs) = cons (Char.toLower s) (lowerStr xs)

-- Lowercase a showable thing
lowerThing :: Show a => a -> String
lowerThing = lowerStr . show

instance WritableUnit SyslogFacility where
  transformUnit = lowerThing

data SyslogLevel = EmergLevel
                 | AlertLevel
                 | CritLevel
                 | ErrLevel
                 | WarningLevel
                 | NoticeLevel
                 | InfoLevel
                 | DebugLevel
                 deriving (Eq, Show)

instance WritableUnit SyslogLevel where
  transformUnit EmergLevel = "emerg"
  transformUnit AlertLevel = "alert"
  transformUnit CritLevel = "crit"
  transformUnit ErrLevel = "err"
  transformUnit WarningLevel = "warning"
  transformUnit NoticeLevel = "notice"
  transformUnit InfoLevel = "info"
  transformUnit DebugLevel = "debug"

data SecureBits = KeepCaps
                | KeepCapsLocked
                | NoSetuidFixup
                | NoSetuidFixupLocked
                | NoRoot
                | NoRootLocked
                deriving (Eq, Show)

instance WritableUnit SecureBits where
  transformUnit KeepCaps = "keep-caps"
  transformUnit KeepCapsLocked = "keep-caps-locked"
  transformUnit NoSetuidFixup = "no-setuid-fixup"
  transformUnit NoSetuidFixupLocked = "no-setuid-fixup-locked"
  transformUnit NoRoot = "noroot"
  transformUnit NoRootLocked = "noroot-locked"

data ProtectFull = ProtectFull deriving (Eq, Show)

instance WritableUnit ProtectFull where
  transformUnit _ = "full"

data ProtectReadOnly = ProtectReadOnly deriving (Eq, Show)

instance WritableUnit ProtectReadOnly where
  transformUnit _ = "read-only"

data MountFlags = Shared | Slave | Private deriving (Eq, Show)

instance WritableUnit MountFlags where
  transformUnit = lowerThing

data UTMPMode = UInit | ULogin | UUser deriving (Eq, Show)

instance WritableUnit UTMPMode where
  transformUnit UInit = "init"
  transformUnit ULogin = "login"
  transformUnit UUser = "user"

data SystemArchitecture = X86 | X86_64 | X32 | ARM deriving (Eq, Show)

instance WritableUnit SystemArchitecture where
  transformUnit X86_64 = "x86-64"
  transformUnit u = lowerThing u

data Personality = Px86 | Px86_64 | PPC | PPC_LE |
                   PPC64 | PPC64_LE | S390 | S390X
                 deriving (Eq, Show)

instance WritableUnit Personality where
  transformUnit Px86 = "x86"
  transformUnit Px86_64 = "x86-64"
  transformUnit PPC = "ppc"
  transformUnit PPC_LE = "ppc-le"
  transformUnit PPC64 = "ppc64"
  transformUnit PPC64_LE = "ppc64-le"
  transformUnit S390 = "s390"
  transformUnit S390X = "s390x"

data ExecutionEnv =
  ExecutionEnv { _workingDirectory :: Maybe FilePath
               , _rootDirectory :: Maybe FilePath
               , _execUser :: Maybe User
               -- , _supplementaryGroups :: [Group]
               , _execNice :: Maybe Int -- -19 to 20
               , _oomScoreAdjust :: Maybe Int -- -1000 to 1000
                                    -- 0 to 3 if Int
               , _ioSchedulingClass :: Maybe (Either Int IOSchedulingClass)
               , _ioSchedulingPriority :: Maybe Int -- 0 to 7
               , _cpuSchedulingPolicy :: Maybe CPUSchedulingPolicy
               , _cpuSchedulingPriority :: Maybe Int -- 1 to 99
               , _cpuSchedulingResetOnFork :: Maybe Bool
               , _cpuAffinity :: Maybe String -- TODO: parse this
               , _uMask :: Maybe String -- TODO: use Mode and coerce to octal string
               , _execEnvironment :: Maybe [String]
               , _environmentFile :: Maybe FilePath
               , _passEnvironment :: Maybe [String]
               , _standardInput :: Maybe StandardInput
               , _standardOutput :: Maybe StandardOutput
               , _standardError :: Maybe StandardOutput -- not a typo
               , _ttyPath :: Maybe FilePath
               , _ttyReset :: Maybe Bool
               , _ttyVHangup :: Maybe Bool
               , _ttyVTDisallocate :: Maybe Bool
               , _syslogIdentifier :: Maybe String
               , _syslogFacility :: Maybe SyslogFacility
               , _syslogLevel :: Maybe SyslogLevel
               , _syslogLevelPrefix :: Maybe Bool
               , _timerSlackNanos :: Maybe Int
               , _limitCPU :: Maybe Int
               , _limitFSIZE :: Maybe Int
               , _limitDATA :: Maybe Int
               , _limitSTACK :: Maybe Int
               , _limitCORE :: Maybe Int
               , _limitRSS :: Maybe Int
               , _limitNOFILE :: Maybe Int
               , _limitAS :: Maybe Int
               , _limitNPROC :: Maybe Int
               , _limitMEMLOCK :: Maybe Int
               , _limitLOCKS :: Maybe Int
               , _limitSIGPENDING :: Maybe Int
               , _limitMSGQUEUE :: Maybe Int
               , _limitNICE :: Maybe Int
               , _limitRTPRIO :: Maybe Int
               , _limitRTTIME :: Maybe Int
               , _pamName :: Maybe String
               , _capabilityBoundingSet :: Maybe [String]
               , _ambientCapabilities :: Maybe [String]
               , _secureBits :: Maybe SecureBits
               , _readWriteDirectories :: Maybe [FilePath]
               , _readOnlyDirectories :: Maybe [FilePath]
               , _inaccessibleDirectories :: Maybe [FilePath]
               , _privateTmp :: Maybe Bool
               , _privateDevices :: Maybe Bool
               , _privateNetwork :: Maybe Bool
               , _protectSystem :: Maybe (Either Bool ProtectFull)
               , _protectHome :: Maybe (Either Bool ProtectReadOnly)
               , _mountFlags :: Maybe MountFlags
               , _utmpIdentifier :: Maybe String -- TODO: ensure 4 chars
               , _utmpMode :: Maybe UTMPMode
               , _seLinuxContext :: Maybe String
               , _appArmorProfile :: Maybe String
               , _smackProcessLabel :: Maybe String
               , _ignoreSigpipe :: Maybe Bool
               , _noNewPrivileges :: Maybe Bool
               , _systemCallFilter :: Maybe [String]
               , _systemCallErrorNumber :: Maybe Int
               , _systemCallArchitectures :: Maybe [SystemArchitecture]
               , _restrictAddressFamilies :: Maybe [String]
               , _personality :: Maybe Personality
               , _runtimeDirectory :: Maybe FilePath
               , _runtimeDirectoryMode :: Maybe String
               } deriving (Eq, Show)
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
                                        -- If set, it's a user-specific systemd
                                        -- service, otherwise it's a root
                                        -- service
                     , _unitOwner    :: Maybe User
                     , _unit         :: Maybe UnitSection
                     , _mainSection  :: a
                     , _install      :: Maybe InstallSection
                     }
                   deriving (Eq, Show)

makeLenses ''SystemdUnit

systemdUnit :: String -> a -> SystemdUnit a
systemdUnit n ms =
  SystemdUnit
  { _name = n
  , _unitOwner = Nothing
  , _unit = Nothing
  , _mainSection = ms
  , _install = Nothing
  }

type Service = SystemdUnit ServiceSection

service :: String -> Service
service n = systemdUnit n serviceSection

type Mount   = SystemdUnit MountSection

instance WritableUnit Service where
  transformUnit s =
    writeOptionalSection (_unit s)
    ++ transformUnit (_mainSection s) ++ "\n"
    ++ writeOptionalSection (_install s)

instance WritableUnit Mount where
  transformUnit mount =
    writeOptionalSection (_unit mount)
    ++ transformUnit (_mainSection mount) ++ "\n"
    ++ writeOptionalSection (_install mount)

systemdUnitLocation :: Dir.Directory -> FilePath
systemdUnitLocation = Dir._path

-- | Given an optional user, return the Directory the systemd unit file should
-- go into
systemdUnitDir :: (Maybe User) -> Dir.Directory
systemdUnitDir Nothing =
  Dir.Directory "/etc/systemd/system/" (Mode RWX O O) 0 0
systemdUnitDir (Just u) =
  Dir.Directory
  (u ^. home </> ".config/systemd/user")
  (Mode RWX R R)
  (u ^. uid)
  (u ^. gid)


instance Craftable Service where
  watchCraft svc =
    let d = systemdUnitDir $ svc ^. unitOwner
        fullPath = systemdUnitLocation d </> (_name svc) ++ ".service"
    in
      do
        craft_ d
        w <- watchCraft_ $ file fullPath
          & File.mode .~ Mode RW O O
          & File.ownerID .~ 0
          & File.groupID .~ 0
          & File.strContent .~ transformUnit svc
        when (changed w) $ daemonReload $ svc ^. unitOwner
        return (w, svc)

instance Craftable Mount where
  watchCraft mount =
    let d = systemdUnitDir $ mount ^. unitOwner
        fullPath = systemdUnitLocation d </> (_name mount) ++ ".mount"
    in
      do
        craft_  d
        w <- watchCraft_ $ file fullPath
          & File.mode .~ Mode RW O O
          & File.ownerID .~ 0
          & File.groupID .~ 0
          & File.strContent .~ transformUnit mount
        when (changed w) $ daemonReload $ mount ^. unitOwner
        return (w, mount)

-- =====================================================================

---- Functions to deal with systemd
systemdCtlBin :: FilePath
systemdCtlBin = "/usr/bin/systemctl"

-- | Reload either the Root or user-specific daemon to re-read the systemd unit
-- files
daemonReload :: (Maybe User) -> Craft()
daemonReload Nothing  = exec_ systemdCtlBin ["daemon-reload"]
daemonReload (Just user) =
  let u = (_username user)
      uidStr = show (_uid user)
      -- These variables must be set or `systemctl` does not work with sudo
      env = [("XDG_RUNTIME_DIR","/run/user/" ++ uidStr)
            ,("BUS_SESSION_BUS_ADDRESS",
              "unix:path=/run/user/" ++ uidStr ++ "/bus")]
  in
    withEnv env $ exec_ "sudo"
    ["-u", u, "--", "sh -c " ++ systemdCtlBin ++ " --user daemon-reload"]

start :: ServiceName -> Craft ()
start s = exec_ systemdCtlBin ["start", s]

stop :: ServiceName -> Craft ()
stop s = exec_ systemdCtlBin ["stop", s]

restart :: ServiceName -> Craft ()
restart s = exec_ systemdCtlBin ["restart", s]

-- TODO: need to return 'true' for running and 'false' for not running here
status :: ServiceName -> Craft ()
status s = exec_ systemdCtlBin ["status", s]


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
redshift :: User -> Service
redshift u =
  service "redshift"
  & unit ?~ unitSection "Redshift"
  & unitOwner ?~ u
  & mainSection .~ (serviceSection
                    & execStart ?~ "/usr/bin/redshift -l geoclue2 -t 6500:3700"
                    & execStop ?~ "/usr/bin/pkill redshift"
                    & environment ?~ "DISPLAY=:0"
                    & restartService ?~ RestartAlways
                   )
  & install ?~ (installSection & wantedBy ?~ "default.target")

-- You should then be able to run:
-- putStrLn $ transformUnit redshift <user>
