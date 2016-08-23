{-# LANGUAGE GADTs #-}
module Craft.Config.Yaml where

import Craft
import Craft.Config
import qualified Data.ByteString.Char8 as B8

import Control.Lens
import Data.Yaml


data YamlFormat a = (FromJSON a, ToJSON a) => YamlFormat { _yamlfmt :: a }


instance (FromJSON a, ToJSON a) => ConfigFormat (YamlFormat a) where
  showConfig = B8.unpack . encode . _yamlfmt
  parse fp s =
    case decodeEither (B8.pack s) of
      Left err -> $craftError $ "Failed to parse " ++ show fp ++ " : " ++ err
      Right x  -> return $ YamlFormat x


get :: (FromJSON a, ToJSON a) => Path Abs FileP -> Craft (Maybe (Config (YamlFormat a)))
get = Craft.Config.get


config :: (FromJSON a, ToJSON a) => Path Abs FileP -> a -> Config (YamlFormat a)
config fp = Craft.Config.config fp . YamlFormat


makeLenses ''YamlFormat
