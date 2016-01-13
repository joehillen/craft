module Craft.File.Sourced where


import           Craft.Internal
import           Craft.File.Mode
import           Craft.User (User, UserID)
import qualified Craft.User as User
import           Craft.Group (Group, GroupID)
import qualified Craft.Group as Group
import           Craft.Internal.FileDirectory
import qualified Craft.File as File

import Control.Lens
import           System.FilePath


data SourcedFile
  = SourcedFile
    { _file    :: File.File
    , _source  :: FilePath
    }
  deriving (Eq, Show)

makeLenses ''SourcedFile


sourcedFile :: FilePath -> FilePath -> SourcedFile
sourcedFile fp source' =
  SourcedFile
  { _file = File.file fp
  , _source = source'
  }


instance Craftable SourcedFile where
  craft_ sf = do
    let sf' = sf & file . File.content .~ Nothing
    sourceFile (sf' ^. source) (sf' ^. file . File.path)
    craft_ $ sf' ^. file

  watchCraft sf = $notImplemented $ "watchCraft " ++ show sf
