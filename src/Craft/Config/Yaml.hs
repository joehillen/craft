{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
module Craft.Config.Yaml where

import           Craft
import           Craft.Config
import qualified Data.ByteString.Char8 as B8

import           Control.Lens
import           Data.Yaml


data YamlFormat a = (FromJSON a, ToJSON a) => YamlFormat { _yamlfmt :: a }


instance (FromJSON a, ToJSON a) => ConfigFormat (YamlFormat a) where
  showConfig (YamlFormat c) = B8.unpack $ encode c
  parseConfig fp s =
    case decodeEither s of
      Left err -> $craftError $ "Failed to parse " ++ show fp ++ " : " ++ err
      Right x  -> return $ YamlFormat x


get :: (FromJSON a, ToJSON a) => AbsFilePath -> Craft (Maybe (Config (YamlFormat a)))
get = Craft.Config.get


config :: (FromJSON a, ToJSON a) => AbsFilePath -> a -> Config (YamlFormat a)
config fp = Craft.Config.config fp . YamlFormat


yamlfmt :: (FromJSON a, ToJSON a) => Lens' (YamlFormat a) a
yamlfmt f (YamlFormat x) = fmap YamlFormat (f x)
