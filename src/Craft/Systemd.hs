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
renderSection (name, fields) = unlines $ ("["++name++"]"):(map renderField fields)

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

data Unit
  = Unit
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

serviceFileName :: Lens' Service RelFilePath
serviceFileName =
  lens
    (view $ path.to filename)
    (\s fn -> s & path .~ ((s^.path.to parent)</>fn))

serviceCommand :: String -> Service -> Craft ()
serviceCommand cmd s = systemctl cmd [fromRelFile $ s^.serviceFileName]

start :: Service -> Craft ()
start = serviceCommand "start"

restart :: Service -> Craft ()
restart = serviceCommand "restart"

stop :: Service -> Craft ()
stop = serviceCommand "stop"

enable :: Service -> Craft ()
enable = serviceCommand "enable"

disable :: Service -> Craft ()
disable = serviceCommand "disable"

reload :: Service -> Craft ()
reload = serviceCommand "reload"

reloadOrRestart :: Service -> Craft ()
reloadOrRestart = serviceCommand "reload-or-restart"

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
            else
              if s^.reloadOnChange
                then reloadOrRestart s
                else return ()
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
