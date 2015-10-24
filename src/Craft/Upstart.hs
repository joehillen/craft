module Craft.Upstart where

import           Craft

import           Control.Monad
import           Text.Megaparsec

type ServiceName = String

data Service
  = Service
    { name   :: String
    , status :: String
    }


get :: ServiceName -> Craft (Maybe Service)
get sn = do
  exec "/sbin/status" [sn] >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> return . Just . Service sn
                         . errorLeft
                         $ parse (statusParser sn) "Upstart status" (stdout r)


statusParser :: String -> Parsec String String
statusParser sn = do
  void $ string sn >> space >> some (noneOf "/") >> char '/'
  some $ noneOf ","


errorLeft :: Show a => Either a b -> b
errorLeft (Left m) = error $ "Upstart status parse: " ++ show m
errorLeft (Right x) = x


start :: Service -> Craft ()
start Service{..} =
  when (status /= "running") $ exec_ "/sbin/start" [name]


restart :: Service -> Craft ()
restart Service{..} =
  exec_ "/sbin/restart" [name]
