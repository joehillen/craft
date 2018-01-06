module Craft.Upstart where

import           Craft

import           Control.Lens         hiding (noneOf)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type ServiceName = String

data Service
  = Service
    { _name   :: String
    , _status :: String
    }

makeLenses ''Service


get :: ServiceName -> Craft (Maybe Service)
get sn =
  exec "/sbin/status" [sn] >>= \case
    Failure _ -> return Nothing
    Success r -> Just . Service sn <$> parseExecResult (Success r) (statusParser sn) (r ^. stdout)


statusParser :: String -> Parsec Void String String
statusParser sn = do
  void $ string sn >> space >> some (noneOf ['/']) >> char '/'
  some $ noneOf [',']


start :: Service -> Craft ()
start Service{..} = when (_status /= "running") $ exec_ "/sbin/start" [_name]


restart :: Service -> Craft ()
restart Service{..} = exec_ "/sbin/restart" [_name]
