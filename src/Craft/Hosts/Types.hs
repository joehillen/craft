module Craft.Hosts.Types where

import Data.Text

-- | This file is just to prevent a cyclic import between Hosts.hs
--  and Hosts/Parser.hs

newtype IP = IP Text
  deriving (Eq)


instance Show IP where
  show (IP ip) = unpack ip


newtype Name = Name Text
  deriving (Eq)


instance Show Name where
  show (Name name) = unpack name


type Configs = [(IP, [Name])]
data Hosts = Hosts { configs :: Configs }
             deriving (Eq, Show)


hostsfp :: FilePath
hostsfp = "/etc/hosts"
