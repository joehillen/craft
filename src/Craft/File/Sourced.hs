module Craft.File.Sourced where

import Control.Lens

import Craft.Internal
import qualified Craft.File as File


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


instance Craftable SourcedFile SourcedFile where
  craft sf = do
    let sf' = sf & file . File.content .~ Nothing
    sourceFile (sf' ^. source) (sf' ^. file . File.path)
    craft_ $ sf' ^. file
    return sf'

  watchCraft sf = do
    let fp = sf ^. file . File.path
    let sf' = sf & file . File.content .~ Nothing
    exists <- File.exists fp
    if exists then do
      oldsum <- File.md5sum fp
      sourceFile (sf ^. source) fp
      fw <- watchCraft_ $ sf' ^. file
      newsum <- File.md5sum fp
      if oldsum /= newsum then
        return (Updated, sf')
      else
        return (fw, sf')
    else do
      sourceFile (sf ^. source) fp
      craft_ $ sf' ^. file
      return (Created, sf')
