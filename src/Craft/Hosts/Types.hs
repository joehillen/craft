module Craft.Hosts.Types where

-- | This file is just to prevent a cyclic import between Hosts.hs
--  and Hosts/Parser.hs

newtype IP = IP String
  deriving (Eq)


instance Show IP where
  show (IP ip) = ip


newtype Name = Name String
  deriving (Eq)


instance Show Name where
  show (Name name) = name


type Configs = [(IP, [Name])]
data Hosts = Hosts { configs :: Configs }
             deriving (Eq, Show)


hostsfp :: FilePath
hostsfp = "/etc/hosts"
