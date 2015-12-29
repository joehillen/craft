module Craft.Config.Ini
( module Craft.Config.Ini
, fromList
)
where

import Control.Lens
import Data.Ini
import Data.HashMap (fromList)
import qualified Data.Text as T

import Craft.Config
import Craft.Types
import Craft.Log


newtype IniFormat = IniFormat { _inifmt :: Ini }


instance ConfigFormat IniFormat where
  showConfig = T.unpack . printIni . _inifmt
  parse fp s =
    case parseIni (T.pack s) of
      Left err -> $craftError $ "Failed to parse ini file" ++ fp ++ ": " ++ err
      Right x  -> return $ IniFormat x


get :: FilePath -> Craft (Maybe (Config IniFormat))
get = Craft.Config.get


lookup :: String -> String -> IniFormat -> Maybe String
lookup section key iniformat =
  case lookupValue (T.pack section) (T.pack key) (_inifmt iniformat) of
    Left  _ -> Nothing
    Right r -> Just $ T.unpack r


makeLenses ''IniFormat
