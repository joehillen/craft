module Craft.Config.Json where

import Craft
import Craft.File (File)
import qualified Craft.File as File
import Craft.File.Mode
import Craft.User (UserID)
import Craft.Group (GroupID)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as B8

import Data.Aeson


data Config cfgs
  = Config
    { path    :: File.Path
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


config :: File.Path -> cfgs -> Config cfgs
config fp cfgs = let f = File.file fp
                 in Config { path = File.path f
                           , mode = File.mode f
                           , ownerID = File.ownerID f
                           , groupID = File.groupID f
                           , configs = cfgs
                           }


configFromFile :: FromJSON cfgs => File -> Config cfgs
configFromFile f =
  Config { path    = File.path f
         , mode    = File.mode f
         , ownerID = File.ownerID f
         , groupID = File.groupID f
         , configs =
             case File.content f of
               Nothing -> error $ "Unmanaged Yaml config: " ++ File.path f
               Just bs -> case eitherDecodeStrict bs of
                            Left err -> error $ "Failed to parse "
                                                ++ File.path f ++ " : " ++ err
                            Right x  -> x
         }


fileFromConfig :: ToJSON cfgs => Config cfgs -> File
fileFromConfig cfg =
  File.File { File.path    = path cfg
            , File.mode    = mode cfg
            , File.ownerID = ownerID cfg
            , File.groupID = groupID cfg
            , File.content = Just . BSL.toStrict. encode $ configs cfg
            }


get :: (FromJSON cfgs) => File.Path -> Craft (Maybe (Config cfgs))
get fp = fmap configFromFile <$> File.get fp


instance (Eq cfg, ToJSON cfg, FromJSON cfg) => Craftable (Config cfg) where
  checker = get . path
  destroyer cfg = destroyer $ fileFromConfig cfg
  crafter cfg mcfg =
    crafter (fileFromConfig cfg) (fileFromConfig <$> mcfg)