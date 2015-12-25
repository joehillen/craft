module Craft.Config.Ini
( module Craft.Config.Ini
, fromList
)
where

import Control.Lens
import Data.Ini
import Data.HashMap (fromList)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Craft.Types
import Craft.Log
import Craft.File (File)
import qualified Craft.File as File
import Craft.File.Mode
import Craft.User (UserID)
import Craft.Group (GroupID)


instance Eq Ini where
  a == b = show a == show b


data Config
  = Config
    { _path    :: FilePath
    , _mode    :: Mode
    , _ownerID :: UserID
    , _groupID :: GroupID
    , _configs :: Ini
    }
    deriving (Show, Eq)
makeLenses ''Config


config :: FilePath -> Ini -> Config
config fp cfgs = let f = File.file fp
                 in Config { _path = f ^. File.path
                           , _mode = f ^. File.mode
                           , _ownerID = f ^. File.ownerID
                           , _groupID = f ^. File.groupID
                           , _configs = cfgs
                           }


configFromFile :: File -> Config
configFromFile f =
  Config { _path    = f ^. File.path
         , _mode    = f ^. File.mode
         , _ownerID = f ^. File.ownerID
         , _groupID = f ^. File.groupID
         , _configs =
             case f ^. File.content of
               Nothing -> error $ "Unmanaged Ini config: " ++ f ^. File.path
               Just bs -> case parseIni (decodeUtf8 bs) of
                            Left err -> error $ "Failed to parse "
                                                ++ f ^. File.path ++ " : " ++ err
                            Right x  -> x
         }


fileFromConfig :: Config -> File
fileFromConfig cfg =
  File.file (cfg ^. path)
    & File.path .~ (cfg ^. path)
    & File.mode    .~ (cfg ^. mode)
    & File.ownerID .~ (cfg ^. ownerID)
    & File.groupID .~ (cfg ^. groupID)
    & File.content ?~ (encodeUtf8 . printIni $ cfg ^. configs)


get :: FilePath -> Craft (Maybe Config)
get fp = fmap configFromFile <$> File.get fp


instance Craftable Config where
  watchCraft cfg = do
    (w, f) <- watchCraft $ fileFromConfig cfg
    return (w, configFromFile f)

{-
instance Destroyable Config where
  watchDestroy cfg = do
    (w, mbf) <- watchDestroy $ fileFromConfig cfg
    case mbf of
      Nothing -> return (w, Nothing)
      Just f -> return (w, Just $ configFromFile f)
-}
