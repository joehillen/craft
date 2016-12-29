module Craft.Systemd where

import Control.Lens
import Craft

data Unit
  = Unit
  { _unitFile       :: File
  , _enabled        :: Bool
  , _status         :: Status
  , _reloadOnChange :: Bool
  }

unit :: File -> Unit
unit f =
  Unit
  { _unitFile = f
  , _enabled = True
  , _status = Running
  , _reloadOnChange = True
  }

type UnitFormat = [(String, [(String, String)])]

data Status
  = Running
  | Stopped

makeLenses ''Unit

renderUnit :: UnitFormat -> String
renderUnit = unlines . map renderSection

renderSection :: (String, [(String, String)]) -> String
renderSection (name, fields) = unlines $ ("["++name++"]"):(map renderField fields)

renderField :: (String, String) -> String
renderField (k, v) = k++"="++v


systemctl :: String -> Args -> Craft ()
systemctl cmd args = exec_ "systemctl" $ cmd:args


instance Craftable Unit Unit where
  watchCraft u = do
    w <- watchCraft_ $ u^.unitFile
    let name = fromRelFile $ filename $ u^.unitFile.path
    when (changed w) $ systemctl "daemon-reload" []
    if u^.enabled
      then systemctl "enable" [name]
      else systemctl "disable" [name]
    case u^.status of
      Stopped -> systemctl "stop" [name]
      Running -> do
        if (u^.reloadOnChange && changed w)
          then systemctl "reload-or-restart" [name]
          else systemctl "start" [name]
    return (w, u)


systemDP :: AbsDirPath
systemDP = $(mkAbsDir "/etc/systemd/system")


service :: String -> UnitFormat -> Craft Unit
service name content = do
  fn <- parseRelFile $ name++".service"
  return $
    unit $
      file (systemDP</>fn)
      & strContent .~ renderUnit content


timer :: String -> UnitFormat -> Craft Unit
timer name content = do
  fn <- parseRelFile $ name++".timer"
  return $
    unit $
      file (systemDP</>fn)
      & strContent .~ renderUnit content
