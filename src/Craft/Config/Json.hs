{-# LANGUAGE GADTs #-}
module Craft.Config.Json where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)

import           Craft
import           Craft.Config
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as B8


data JsonFormat a = (FromJSON a, ToJSON a) => JsonFormat { _jsonfmt :: a }


instance (FromJSON a, ToJSON a) => ConfigFormat (JsonFormat a) where
  showConfig = B8.unpack . BSL.toStrict . encodePretty . _jsonfmt
  parseConfig fp s =
    case eitherDecodeStrict (B8.pack s) of
      Left err -> $craftError $ "Failed to parse " ++ fp ++ " : " ++ err
      Right x  -> return $ JsonFormat x


get :: (FromJSON a, ToJSON a) => FilePath -> Craft (Maybe (Config (JsonFormat a)))
get = Craft.Config.get


config :: (FromJSON a, ToJSON a) => FilePath -> a -> Config (JsonFormat a)
config fp = Craft.Config.config fp . JsonFormat


makeLenses ''JsonFormat
