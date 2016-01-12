{-# LANGUAGE PackageImports #-}

module Craft.Config.Ini
( module Craft.Config.Ini
, Ini(..)
, HM.fromList
)
where

import Control.Lens
import Data.Ini
import qualified "unordered-containers" Data.HashMap.Strict as HM
import qualified Data.Text as T

import Craft.Config
import Craft.Types
import Craft.Log


newtype IniFormat = IniFormat { _inifmt :: Ini }


config :: FilePath -> Ini -> Config IniFormat
config fp ini = Craft.Config.config fp (IniFormat ini)


instance ConfigFormat IniFormat where
  showConfig = T.unpack . printIni . _inifmt
  parse fp s =
    case parseIni (T.pack s) of
      Left err -> $craftError $ "Failed to parse ini file" ++ fp ++ ": " ++ err
      Right x  -> return $ IniFormat x


get :: FilePath -> Craft (Maybe (Config IniFormat))
get = Craft.Config.get


lookup :: String -> String -> IniFormat -> Maybe String
lookup section key (IniFormat ini)=
  case lookupValue (T.pack section) (T.pack key) ini of
    Left  _ -> Nothing
    Right r -> Just $ T.unpack r


makeLenses ''IniFormat
