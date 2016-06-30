module Craft.Group where

import Control.Lens
import           Data.List (intercalate)

import           Craft.Internal
import           Craft.Internal.Helpers
import           Craft.Internal.UserGroup

type Name = GroupName

name :: Lens' Group GroupName
name = groupName

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
  exec_ "groupadd" args
  exec_ "gpasswd" [ "--members", intercalate "," (map show optUsers)
                           , show gn]
  fromName gn >>= \case
    Nothing -> $craftError $ "createGroup `" ++ show gn ++ "` failed. Not Found!"
    Just g -> return g
 where
  args = concat
   [ toArg "--gid"        optGID
   , toArg "--non-unique" optAllowdupe
   , toArg "--system"     optSystem
   ]

fromName :: GroupName -> Craft (Maybe Group)
fromName (GroupName s) = groupFromStr s

fromID :: GroupID -> Craft (Maybe Group)
fromID = groupFromID
