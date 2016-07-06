module Craft.File.Sourced where

import           Control.Lens

import           Craft
import qualified Craft.File as File


data SourcedFile
  = SourcedFile
    { _file    :: File
    , _source  :: FilePath
    }
  deriving (Eq, Show)

makeLenses ''SourcedFile

-- TODO: instance FileLike SourcedFile

sourcedFile :: FilePath -> FilePath -> SourcedFile
sourcedFile fp source' =
  SourcedFile
  { _file = Craft.file fp
  , _source = source'
  }



instance Craftable SourcedFile SourcedFile where
  craft sf = do
    let sf' = sf & Craft.File.Sourced.file . fileContent .~ Nothing
    sourceFile (sf' ^. source) (sf' ^. Craft.File.Sourced.file . filePath)
    craft_ $ sf' ^. Craft.File.Sourced.file
    return sf'

  watchCraft sf = do
    let fp = sf ^. Craft.File.Sourced.file . filePath
    let sf' = sf & Craft.File.Sourced.file . fileContent .~ Nothing
    exists <- File.exists fp
    if exists then do
      oldsum <- File.md5sum fp
      sourceFile (sf ^. source) fp
      fw <- watchCraft_ $ sf' ^. Craft.File.Sourced.file
      newsum <- File.md5sum fp
      if oldsum /= newsum then
        return (Updated, sf')
      else
        return (fw, sf')
    else do
      sourceFile (sf ^. source) fp
      craft_ $ sf' ^. Craft.File.Sourced.file
      return (Created, sf')


instance Destroyable SourcedFile where
  watchDestroy sf = do
    (w, mbf) <- watchDestroy $ sf^.Craft.File.Sourced.file
    let res = case mbf of
                Nothing -> Nothing
                Just f  -> Just $ sf & Craft.File.Sourced.file .~ f
    return (w, res)
