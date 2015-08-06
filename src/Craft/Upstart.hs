module Craft.Upstart where

import           Craft

import           Control.Monad
import           Text.Parsec
import           Text.Parsec.String (Parser)

type ServiceName = String

data Service
  = Service
    { name   :: String
    , status :: String
    }

get :: ServiceName -> Craft (Maybe Service)
get sn = do
  (exitcode, stdout, _stderr) <- exec "/sbin/status" [sn]
  case exitcode of
    ExitSuccess -> do
      let stat = errorLeft $ parse (statusParser sn) "Upstart status" stdout
      return . Just $ Service sn stat
    ExitFailure _ -> return Nothing

statusParser :: String -> Parser String
statusParser sn = do
  void $ string sn >> space >> many1 (noneOf "/") >> char '/'
  many1 $ noneOf ","

errorLeft :: Show a => Either a b -> b
errorLeft (Left m) = error $ "Upstart status parse: " ++ show m
errorLeft (Right x) = x

start :: Service -> Craft ()
start Service{..} =
  when (status /= "running") $ exec_ "/sbin/start" [name]

restart :: Service -> Craft ()
restart Service{..} =
  exec_ "/sbin/restart" [name]
