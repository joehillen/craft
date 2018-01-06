{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
module Craft.Config.Json where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)

import           Craft
import           Craft.Config
import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Lazy     as BSL


data JsonFormat a = (FromJSON a, ToJSON a) => JsonFormat { _jsonfmt :: a }


instance (FromJSON a, ToJSON a) => ConfigFormat (JsonFormat a) where
  showConfig = B8.unpack . BSL.toStrict . encodePretty . _jsonfmt
  parseConfig fp s =
    case eitherDecodeStrict s of
      Left err -> $craftError $ "Failed to parse "++show fp++" : "++err
      Right x  -> return $ JsonFormat x


get :: (FromJSON a, ToJSON a) => AbsFilePath -> Craft (Maybe (Config (JsonFormat a)))
get = Craft.Config.get


config :: (FromJSON a, ToJSON a) => AbsFilePath -> a -> Config (JsonFormat a)
config fp = Craft.Config.config fp . JsonFormat


jsonfmt :: (FromJSON a, ToJSON a) => Lens' (JsonFormat a) a
jsonfmt f (JsonFormat x) = fmap JsonFormat (f x)
