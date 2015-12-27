module Craft.Config.Json where

import Craft
import Craft.File (File)
import qualified Craft.File as File
import Craft.File.Mode
import Craft.User (UserID)
import Craft.Group (GroupID)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as B8

import Control.Lens
import Data.Aeson


data Config cfgs
  = Config
    { path    :: FilePath
    , mode    :: Mode
    , ownerID :: UserID
    , groupID :: GroupID
    , configs :: cfgs
    }
    deriving (Eq)


instance ToJSON cfg => Show (Config cfg) where
  show f = "Yaml.Config " ++
           "{ path = " ++ show (path f) ++
           ", mode = " ++ show (mode f) ++
           ", ownerID = " ++ show (ownerID f) ++
           ", groupID = " ++ show (groupID f) ++
           ", configs = \"" ++ content ++ "\"" ++
           "}"
   where
    content = B8.unpack . BSL.toStrict . encode $ configs f


config :: FilePath -> cfgs -> Config cfgs
config fp cfgs = let f = File.file fp
                 in Config { path = f ^. File.path
                           , mode = f ^. File.mode
                           , ownerID = f ^. File.ownerID
                           , groupID = f ^. File.groupID
                           , configs = cfgs
                           }


configFromFile :: FromJSON cfgs => File -> Craft (Config cfgs)
configFromFile f = do
  cfgs <- case f ^. File.content of
            Nothing -> $craftError $ "Unmanaged Yaml config: " ++ f ^. File.path
            Just bs -> case eitherDecodeStrict bs of
                        Left err ->
                          $craftError $ "Failed to parse "
                                      ++ f ^. File.path ++ " : " ++ err
                        Right x  -> return x
  return Config { path    = f ^. File.path
                , mode    = f ^. File.mode
                , ownerID = f ^. File.ownerID
                , groupID = f ^. File.groupID
                , configs = cfgs
                }


fileFromConfig :: ToJSON cfgs => Config cfgs -> File
fileFromConfig cfg =
  File.file (path cfg) & File.mode    .~ mode cfg
                       & File.ownerID .~ ownerID cfg
                       & File.groupID .~ groupID cfg
                       & File.content ?~ (BSL.toStrict . encode $ configs cfg)


get :: (FromJSON cfgs) => FilePath -> Craft (Maybe (Config cfgs))
get fp =
  File.get fp >>= \case
    Nothing -> return Nothing
    Just f  -> Just <$> configFromFile f


instance (Eq cfg, ToJSON cfg, FromJSON cfg) => Craftable (Config cfg) where
  watchCraft cfg = do
    w <- watchCraft_ $ fileFromConfig cfg

    return (w, cfg)
