module Craft.Group
( module Craft.Group
, Group(..)
, GroupID
)
where

import           Data.List (intercalate)

import           Craft.Internal
import           Craft.Internal.Helpers
import           Craft.Internal.UserGroup

type Name = GroupName

name :: Group -> Name
name = groupname

data Options =
  Options
  { optGID :: Maybe GroupID
  , optAllowdupe :: Bool
  , optUsers :: [UserName]
  , optSystem :: Bool
  }

opts :: Options
opts =
  Options
  { optGID       = Nothing
  , optAllowdupe = False
  , optUsers     = []
  , optSystem    = False
  }

createGroup :: Name -> Options -> Craft Group
createGroup gn Options{..} = do
  exec_ "/usr/sbin/groupadd" args
  exec_ "/usr/bin/gpasswd" ["--members", intercalate "," optUsers, gn]
  fromName gn >>= \case
    Nothing -> $craftError $ "createGroup `" ++ show gn ++ "` failed. Not Found!"
    Just g -> return g
 where
  args = concat
   [ toArg "--gid"        optGID
   , toArg "--non-unique" optAllowdupe
   , toArg "--system"     optSystem
   ]

fromName :: Name -> Craft (Maybe Group)
fromName = groupFromName

fromID :: GroupID -> Craft (Maybe Group)
fromID = groupFromID
