{-# LANGUAGE GADTs #-}
module Craft.Config.Json where

import Craft
import Craft.Config
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as B8

import Control.Lens
import Data.Aeson

data JsonFormat a = (FromJSON a, ToJSON a) => JsonFormat { _jsonfmt :: a }


instance (FromJSON a, ToJSON a) => ConfigFormat (JsonFormat a) where
  showConfig = B8.unpack . BSL.toStrict . encode . _jsonfmt
  parse fp s =
    case eitherDecodeStrict (B8.pack s) of
      Left err -> $craftError $ "Failed to parse " ++ fp ++ " : " ++ err
      Right x  -> return $ JsonFormat x


get :: (FromJSON a, ToJSON a) => FilePath -> Craft (Maybe (Config (JsonFormat a)))
get = Craft.Config.get


makeLenses ''JsonFormat
