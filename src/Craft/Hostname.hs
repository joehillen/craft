module Craft.Hostname where

import qualified Data.ByteString.Char8 as B8

import qualified Craft.File as File
import qualified Craft.Hosts as Hosts
import Craft.Internal


data Hostname = Hostname String
  deriving (Eq, Show)


get :: Craft Hostname
get = Hostname . stdout . errorOnFail <$> exec "hostname" []


instance Craftable Hostname where
  checker _ = Just <$> get
  destroyer _ = return ()
  crafter (Hostname h) _ = do
    exec_ "hostname" [h]
    Hosts.get >>= \case
      Nothing -> do
        $logError "/etc/hosts not found!"
        error "/etc/hosts not found!"
      Just hosts ->
        craft_ $ Hosts.set (Hosts.Name h) (Hosts.IP "127.0.1.1") hosts
    craft_ $ (File.file "/etc/hostname")
             { File.content = Just $ B8.pack h }
