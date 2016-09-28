module Craft.Config.Ini
( module Craft.Config.Ini
, module Data.Ini
, Ini(..)
, HM.fromList
)
where

import           Control.Lens
import qualified Data.HashMap.Strict as HM
import           Data.Ini
import qualified Data.Text           as T

import           Craft.Config
import           Craft.Types


data IniFormat = IniFormat { _inifmt   :: Ini
                           , _settings :: WriteIniSettings
                           }


iniFormat :: [(T.Text, [(T.Text, T.Text)])] -> IniFormat
iniFormat ini = IniFormat (Ini (HM.fromList $ fmap (fmap HM.fromList) ini))
                          (WriteIniSettings EqualsKeySeparator)


config :: Path Abs FileP -> IniFormat -> Config IniFormat
config = Craft.Config.config


instance ConfigFormat IniFormat where
  showConfig format = T.unpack . printIniWith (_settings format) $ _inifmt format
  parseConfig fp s =
    case parseIni (T.pack s) of
      Left err -> $craftError $ "Failed to parse ini file"++show fp++": "++err
      Right x  -> return $ IniFormat
                           { _inifmt   = x
                           , _settings = WriteIniSettings EqualsKeySeparator
                           }


get :: Path Abs FileP -> Craft (Maybe (Config IniFormat))
get = Craft.Config.get


lookup :: String -> String -> IniFormat -> Maybe String
lookup section key (IniFormat ini _)=
  case lookupValue (T.pack section) (T.pack key) ini of
    Left  _ -> Nothing
    Right r -> Just $ T.unpack r


makeLenses ''IniFormat
