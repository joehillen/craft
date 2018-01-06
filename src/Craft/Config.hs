module Craft.Config where

import           Control.Lens
import           Data.ByteString       (ByteString)

import           Craft
import qualified Craft.File            as File


data Config a
  = Config
    { _configFile :: File
    , _configs    :: a
    }


config :: ConfigFormat a => AbsFilePath -> a -> Config a
config fp cfg =
  Config
  { _configFile = file fp & strContent .~ showConfig cfg
  , _configs    = cfg
  }


class ConfigFormat a where
  showConfig :: a -> String

  parseConfig :: AbsFilePath -> ByteString -> Craft a

  configFromFile :: File -> Craft (Config a)
  configFromFile f = do
    bs <- case f ^. fileContent of
            Nothing -> fileRead (f ^. path)
            Just bs -> return bs
    cfgs <- parseConfig (f^.path) bs
    return Config { _configFile = f
                  , _configs    = cfgs
                  }

  fileFromConfig :: Config a -> Craft File
  fileFromConfig cfg =
    return $ _configFile cfg & strContent .~ showConfig (_configs cfg) ++ "\n"


  {-# MINIMAL showConfig, parseConfig #-}

makeLenses ''Config


instance FileLike (Config a) where
  type FileLikePath (Config a) = AbsFilePath
  path = configFile . filePath
  mode = configFile . fileMode
  ownerID = configFile . fileOwnerID
  groupID = configFile . fileGroupID


instance ConfigFormat a => Craftable (Config a) (Config a) where
  watchCraft cfg = do
    w <- watchCraft_ =<< fileFromConfig cfg
    return (w, cfg)


instance ConfigFormat a => Show (Config a) where
  show cfg = "Config "
          ++ "{ _configFile = " ++ show (_configFile cfg)
          ++ ", _configs = \"" ++ showConfig (_configs cfg) ++ "\""
          ++ "}"


get :: ConfigFormat a => AbsFilePath -> Craft (Maybe (Config a))
get fp =
  File.getWithContent fp >>= \case
    Nothing -> return Nothing
    Just f  -> Just <$> configFromFile f
