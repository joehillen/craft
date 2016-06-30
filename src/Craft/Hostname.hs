module Craft.Hostname where

import           Control.Lens
import           Control.Monad.Logger (logInfo)
import qualified Data.Text as T

import           Craft
import qualified Craft.Hosts as Hosts


data Hostname = Hostname String
  deriving (Eq, Show)


get :: Craft Hostname
get = Hostname <$> ($stdoutOrError =<< exec "hostname" [])


instance Craftable Hostname Hostname where
  watchCraft (Hostname hn) = do
    (Hostname oldhn) <- get
    hosts <- Hosts.get
    if oldhn /= hn then do
      $logInfo . T.pack $ "Hostname " ++ oldhn ++ " /= " ++ hn
      hosts' <- craft $ Hosts.set (Hosts.Name hn) (Hosts.IP "127.0.1.1") hosts
      craft_ $ file "/etc/hostname" & strContent .~ hn
      exec_ "hostname" [hn]
      craft_ $ Hosts.deleteName (Hosts.Name oldhn) hosts'
      (Hostname newhn) <- get
      when (newhn /= hn) $
        $craftError $ "craft Hostname failed! Expected: " ++ hn
                                         ++ " Got: " ++ newhn
      return (Updated, Hostname hn)
    else do
      craft_ $ file "/etc/hostname" & strContent .~ hn
      case Hosts.lookup (Hosts.IP "127.0.1.1") hosts of
        Just names ->
          if Hosts.Name hn `elem` names then
            return (Unchanged, Hostname hn)
          else do
            w <- watchCraft_ $ Hosts.set (Hosts.Name hn) (Hosts.IP "127.0.1.1") hosts
            return (w, Hostname hn)
        Nothing -> do
          w <- watchCraft_ $ Hosts.set (Hosts.Name hn) (Hosts.IP "127.0.1.1") hosts
          return (w, Hostname hn)
