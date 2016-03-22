module Craft.Config.Ini
( module Craft.Config.Ini
, module Data.Ini
, Ini(..)
, HM.fromList
)
where

import Control.Lens
import Data.Ini
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Craft.Config
import Craft.Types


data IniFormat = IniFormat { _inifmt :: Ini
                           , _settings :: WriteIniSettings
                           }


iniFormat :: Ini -> IniFormat
iniFormat ini = IniFormat ini (WriteIniSettings EqualsKeySeparator)

config :: FilePath -> IniFormat -> Config IniFormat
config = Craft.Config.config


instance ConfigFormat IniFormat where
  showConfig format = T.unpack . printIniWith (_settings format) $ _inifmt format
  parse fp s =
    case parseIni (T.pack s) of
      Left err -> $craftError $ "Failed to parse ini file" ++ fp ++ ": " ++ err
      Right x  -> return $ iniFormat x


get :: FilePath -> Craft (Maybe (Config IniFormat))
get = Craft.Config.get


lookup :: String -> String -> IniFormat -> Maybe String
lookup section key (IniFormat ini _)=
  case lookupValue (T.pack section) (T.pack key) ini of
    Left  _ -> Nothing
    Right r -> Just $ T.unpack r


makeLenses ''IniFormat
