module Craft.Systemd where

import           Control.Lens
import           Craft
import           Data.ByteString     (ByteString)
import           Data.List           (isSuffixOf)
import           Language.Haskell.TH


systemdDP :: AbsDirPath
systemdDP = $(mkAbsDir "/etc/systemd/system")

--------------------------------------------------------------------------------
-- Helpers

systemctl :: String -> Args -> Craft ()
systemctl cmd args = exec_ "systemctl" $ cmd:args

renderUnit :: UnitFormat -> String
renderUnit = unlines . map renderSection

renderSection :: (String, [(String, String)]) -> String
renderSection (name, fields) = unlines $ ("["++name++"]"):map renderField fields

renderField :: (String, String) -> String
renderField (k, v) = k++"="++v


--------------------------------------------------------------------------------
-- Types

type UnitFormat = [(String, [(String, String)])]

data Status
  = Running
  | Stopped


--------------------------------------------------------------------------------
-- Unit

newtype Unit = Unit
  { _unitFile       :: File
  }

unit :: AbsFilePath -> UnitFormat -> Unit
unit fp content =
  Unit
  { _unitFile = file fp & strContent .~ renderUnit content
  }
makeLenses ''Unit

instance FileLike Unit where
  type FileLikePath Unit = AbsFilePath
  path = unitFile.path
  mode = unitFile.mode
  ownerID = unitFile.fileOwnerID
  groupID = unitFile.fileGroupID


parseUnit :: ByteString -> UnitFormat
parseUnit _s = error "Craft.Systemd.parseUnit not implemented!"


unitContent :: Lens' Unit UnitFormat
unitContent =
  lens
    (view $ unitFile . fileContent . _Just . to parseUnit)
    (\u uf -> u & unitFile . strContent .~ renderUnit uf)


instance Craftable Unit Unit where
  watchCraft u = do
    w <- watchCraft_ $ u^.unitFile
    when (changed w) $ systemctl "daemon-reload" []
    return (w, u)

mkUnitPath :: FilePath -> Q Exp
mkUnitPath s = [| systemdDP </> $(mkRelFile s) |]

--------------------------------------------------------------------------------
-- Service

data Service
  = Service
  { _serviceUnit     :: Unit
  , _enabled         :: Bool
  , _status          :: Status
  , _restartOnChange :: Bool
  , _reloadOnChange  :: Bool
  , _watches         :: [Watched]
  }


service :: AbsFilePath -> UnitFormat -> Service
service fp content =
  Service
  { _serviceUnit = unit fp content
  , _enabled         = True
  , _status          = Running
  , _restartOnChange = True
  , _reloadOnChange  = True
  , _watches         = []
  }
makeLenses ''Service

instance FileLike Service where
  type FileLikePath Service = AbsFilePath
  path = serviceUnit.path
  mode = serviceUnit.mode
  ownerID = serviceUnit.ownerID
  groupID = serviceUnit.groupID


mkServicePath :: FilePath -> Q Exp
mkServicePath s =
  let ext = ".service"
      sn =
        if ext `isSuffixOf` s
          then s
          else s++ext
  in [| systemdDP </> $(mkRelFile sn) |]


class HasUnitName a where
  unitName :: Getter a String

instance HasUnitName Unit where
  unitName = unitFile.fileName.to fromRelFile

instance HasUnitName Service where
  unitName = serviceUnit.unitName



ctl :: HasUnitName u => String -> u -> Craft ()
ctl cmd u = systemctl cmd [u^.unitName]

start :: HasUnitName u => u -> Craft ()
start = ctl "start"

restart :: HasUnitName u => u -> Craft ()
restart = ctl "restart"

stop :: HasUnitName u => u -> Craft ()
stop = ctl "stop"

enable :: HasUnitName u => u -> Craft ()
enable = ctl "enable"

enableNow :: HasUnitName u => u -> Craft ()
enableNow u = systemctl "enable" ["--now", u^.unitName]

disable :: HasUnitName u => u -> Craft ()
disable = ctl "disable"

disableNow :: HasUnitName u => u -> Craft ()
disableNow u = systemctl "disable" ["--now", u^.unitName]

reload :: HasUnitName u => u -> Craft ()
reload = ctl "reload"

reloadOrRestart :: HasUnitName u => u -> Craft ()
reloadOrRestart = ctl "reload-or-restart"

isActive :: HasUnitName a => a -> Craft Bool
isActive u = isSuccess <$> exec "systemctl" ["is-active", u^.unitName]

isEnabled :: HasUnitName a => a -> Craft Bool
isEnabled u = isSuccess <$> exec "systemctl" ["is-enabled", u^.unitName]

instance Craftable Service Service where
  watchCraft s = do
    !w <- watchCraft_ $ s^.serviceUnit
    if s^.enabled
      then enable s
      else disable s
    case s^.status of
      Stopped -> stop s
      Running -> do
        when (w /= Created && any changed (w:s^.watches)) $
          if s^.restartOnChange
            then restart s
            else when (s^.reloadOnChange) $ reloadOrRestart s
        start s
    return (w, s)


--------------------------------------------------------------------------------
-- Timer

mkTimerPaths :: FilePath -> Q Exp
mkTimerPaths s = [| (mkUnitPath (s++".service"), mkUnitPath (s++".timer")) |]

craftTimer :: String -> UnitFormat -> UnitFormat -> Craft ()
craftTimer name serviceContent timerContent = do
  tn <- parseRelFile $ name++".timer"
  sn <- parseRelFile $ name++".service"
  craft_ $ unit (systemdDP</>sn) serviceContent
  craft_ $ service (systemdDP</>tn) timerContent
