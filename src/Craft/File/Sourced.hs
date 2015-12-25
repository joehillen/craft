module Craft.File.Sourced where


import           Craft.Internal
import           Craft.File.Mode
import           Craft.User (User, UserID)
import qualified Craft.User as User
import           Craft.Group (Group, GroupID)
import qualified Craft.Group as Group
import           Craft.Internal.FileDirectory
import qualified Craft.File as File

import           System.FilePath


data SourcedFile
  = SourcedFile
    { path    :: FilePath
    , mode    :: Mode
    , ownerID :: UserID
    , groupID :: GroupID
    , source  :: FilePath
    }
  deriving (Eq, Show)
