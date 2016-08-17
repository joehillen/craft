module Craft.File.Sourced where

import           Control.Lens

import           Craft
import qualified Craft.File as File


data SourcedFile
  = SourcedFile
    { _destination :: File
    , _source      :: FilePath
    }
  deriving (Eq, Show)

makeLenses ''SourcedFile


sourcedFile :: FilePath -> FilePath -> SourcedFile
sourcedFile fp source' =
  SourcedFile
  { _destination = Craft.file fp
  , _source = source'
  }


instance FileLike SourcedFile where
  type FileLikePath SourcedFile = FilePath
  path    = destination . filePath
  mode    = destination . fileMode
  ownerID = destination . fileOwnerID
  groupID = destination . fileGroupID


instance Craftable SourcedFile SourcedFile where
  craft sf = do
    let sf' = sf & destination . fileContent .~ Nothing
    sourceFile (sf'^.source) (sf'^.path)
    craft_ $ sf' ^. destination
    return sf'

  watchCraft sf = do
    let fp = sf ^. destination . filePath
    let sf' = sf & destination . fileContent .~ Nothing
    exists <- File.exists fp
    if exists then do
      oldsum <- File.md5sum fp
      sourceFile (sf^.source) fp
      fw <- watchCraft_ $ sf' ^. destination
      newsum <- File.md5sum fp
      if oldsum /= newsum then
        return (Updated, sf')
      else
        return (fw, sf')
    else do
      sourceFile (sf^.source) fp
      craft_ $ sf' ^. destination
      return (Created, sf')


instance Destroyable SourcedFile where
  watchDestroy sf = do
    (w, mbf) <- watchDestroy $ sf^.destination
    let res = case mbf of
                Nothing -> Nothing
                Just f  -> Just $ sf & destination .~ f
    return (w, res)
