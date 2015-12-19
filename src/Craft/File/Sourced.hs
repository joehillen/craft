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


sourcedToFile :: SourcedFile -> File.File
sourcedToFile sf = File.File { File.path    = path sf
                             , File.mode    = mode sf
                             , File.ownerID = ownerID sf
                             , File.groupID = groupID sf
                             , File.content = Nothing
                             }


owner :: SourcedFile -> Craft User
owner = File.owner . sourcedToFile


group :: SourcedFile -> Craft Group
group = File.group . sourcedToFile


craft' :: SourcedFile -> Craft ()
craft' sf = do
  let fp = path sf
  sourceFile (source sf) fp
  setMode (mode sf) fp
  setOwnerID (ownerID sf) fp
  setGroupID (groupID sf) fp
