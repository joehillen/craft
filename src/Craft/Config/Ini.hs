module Craft.Config.Ini
( module Craft.Config.Ini
, fromList
)
where

import Data.Ini
import Data.HashMap (fromList)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Craft.Types
import Craft.File (File)
import qualified Craft.File as File
import Craft.File.Mode
import Craft.User (UserID)
import Craft.Group (GroupID)

data Config
  = Config
    { path    :: File.Path
    , mode    :: Mode
    , ownerID :: UserID
    , groupID :: GroupID
    , configs :: Ini
    }
    deriving (Show, Eq)


instance Eq Ini where
  a == b = show a == show b


config :: File.Path -> Ini -> Config
config fp cfgs = let f = File.file fp
                 in Config { path = File.path f
                           , mode = File.mode f
                           , ownerID = File.ownerID f
                           , groupID = File.groupID f
                           , configs = cfgs
                           }


configFromFile :: File -> Config
configFromFile f =
  Config { path    = File.path f
         , mode    = File.mode f
         , ownerID = File.ownerID f
         , groupID = File.groupID f
         , configs =
             case File.content f of
               Nothing -> error $ "Unmanaged Ini config: " ++ File.path f
               Just bs -> case parseIni (decodeUtf8 bs) of
                            Left err -> error $ "Failed to parse "
                                                ++ File.path f ++ " : " ++ err
                            Right x  -> x
         }


fileFromConfig :: Config -> File
fileFromConfig cfg =
  File.File { File.path    = path cfg
            , File.mode    = mode cfg
            , File.ownerID = ownerID cfg
            , File.groupID = groupID cfg
            , File.content = Just . encodeUtf8 . printIni
                             $ configs cfg
            }


get :: File.Path -> Craft (Maybe Config)
get fp = fmap configFromFile <$> File.get fp


instance Craftable Config where
  checker = get . path
  destroyer cfg = destroyer $ fileFromConfig cfg
  crafter cfg mcfg =
    crafter (fileFromConfig cfg) (fileFromConfig <$> mcfg)
