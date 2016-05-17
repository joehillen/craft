module Craft.Config where

import           Control.Lens
import qualified Data.ByteString.Char8 as B8

import           Craft
import           Craft.User (User, UserID)
import           Craft.Group (GroupID)
import           Craft.File.Mode (Mode)
import           Craft.File (File, file)
import qualified Craft.File as File


data Config a
  = Config
    { _configFile :: File
    , _configs    :: a
    }


config :: ConfigFormat a => FilePath -> a -> Config a
config fp cfg =
  Config { _configFile = file fp & File.strContent .~ showConfig cfg
         , _configs    = cfg
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
    return Config { _configFile = f
                  , _configs    = cfgs
                  }

  fileFromConfig :: Config a -> Craft File
  fileFromConfig cfg =
    return $ _configFile cfg
      & File.strContent .~ showConfig (_configs cfg) ++ "\n"


  {-# MINIMAL showConfig, parse #-}


makeLenses ''Config


path :: Lens' (Config a) FilePath
path = configFile . File.path


mode :: Lens' (Config a) Mode
mode = configFile . File.mode


ownerID :: Lens' (Config a) UserID
ownerID = configFile . File.ownerID


groupID :: Lens' (Config a) GroupID
groupID = configFile . File.groupID


ownerAndGroup :: Setter (Config a) (Config a) () User
ownerAndGroup = configFile . File.ownerAndGroup


instance ConfigFormat a => Craftable (Config a) (Config a) where
  watchCraft cfg = do
    w <- watchCraft_ =<< fileFromConfig cfg
    return (w, cfg)


instance ConfigFormat a => Show (Config a) where
  show cfg = "Config "
          ++ "{ _configFile = " ++ show (_configFile cfg)
          ++ ", _configs = \"" ++ showConfig (_configs cfg) ++ "\""
          ++ "}"


get :: ConfigFormat a => FilePath -> Craft (Maybe (Config a))
get fp =
  File.get fp >>= \case
    Nothing -> return Nothing
    Just f  -> Just <$> configFromFile f
