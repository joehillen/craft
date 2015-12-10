module Craft.Hostname where

import qualified Data.ByteString.Char8 as B8

import qualified Craft.File as File
import qualified Craft.Hosts as Hosts
import Craft.Internal
import Data.List (find)
import Data.Maybe (fromJust)


data Hostname = Hostname String
  deriving (Eq, Show)


get :: Craft Hostname
get = Hostname . stdout . errorOnFail <$> exec "hostname" []


instance Craftable Hostname where
  checker (Hostname hostname) = do
    hosts <- Hosts.get >>= \case
      Nothing -> $craftError "/etc/hosts not found!"
      Just hosts -> return hosts
    actualHostname <- get
    let namesMB = Hosts.lookup (Hosts.IP "127.0.1.1") hosts
    return (namesMB
            >> find (== Hosts.Name hostname) (fromJust namesMB)
            >> Just actualHostname)
  destroyer _ = return ()
  crafter (Hostname h) _ = do
    exec_ "hostname" [h]
    craft_ $ (File.file "/etc/hostname")
             { File.content = Just $ B8.pack h }
    Hosts.get >>= \case
      Nothing -> $craftError "/etc/hosts not found!"
      Just hosts ->
        craft_ $ Hosts.set (Hosts.Name h) (Hosts.IP "127.0.1.1") hosts
