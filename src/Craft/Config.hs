module Craft.Config where

import Control.Lens
import qualified Data.ByteString.Char8 as B8

import           Craft
import           Craft.File (File, file)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.User (UserID)
import           Craft.Group (GroupID)


data Config a
  = Config
    { _path    :: FilePath
    , _mode    :: Mode
    , _ownerID :: UserID
    , _groupID :: GroupID
    , _configs :: ConfigFormat a => a
    }

class ConfigFormat a where
  showConfig :: a -> String

  parse :: FilePath -> String -> Craft a

  configFromFile :: File -> Craft (Config a)
  configFromFile f = do
    bs <- case f ^. File.content of
            Nothing -> fileRead (f ^. File.path)
            Just bs -> return bs
    cfgs <- parse (f ^. File.path) (B8.unpack bs)
    return Config { _path    = f ^. File.path
                  , _mode    = f ^. File.mode
                  , _ownerID = f ^. File.ownerID
                  , _groupID = f ^. File.groupID
                  , _configs = cfgs
                  }

  fileFromConfig :: Config a -> Craft File
  fileFromConfig cfg =
    return $ file (_path cfg) & File.mode    .~ _mode cfg
                              & File.ownerID .~ _ownerID cfg
                              & File.groupID .~ _groupID cfg
                              & File.strContent .~ showConfig (_configs cfg)

  {-# MINIMAL showConfig, parse #-}


makeLenses ''Config


instance ConfigFormat a => Craftable (Config a) where
  watchCraft cfg = do
    w <- watchCraft_ =<< fileFromConfig cfg
    return (w, cfg)

instance ConfigFormat a => Show (Config a) where
  show cfg = "Config "
          ++ "{ path = " ++ show (_path cfg)
          ++ ", mode = " ++ show (_mode cfg)
          ++ ", ownerID = " ++ show (_ownerID cfg)
          ++ ", groupID = " ++ show (_groupID cfg)
          ++ ", configs = \"" ++ showConfig (_configs cfg) ++ "\""
          ++ "}"


get :: ConfigFormat a => FilePath -> Craft (Maybe (Config a))
get fp =
  File.get fp >>= \case
    Nothing -> return Nothing
    Just f  -> Just <$> configFromFile f
