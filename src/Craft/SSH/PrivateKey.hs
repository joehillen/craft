module Craft.SSH.PrivateKey where

import           Control.Lens

import           Craft
import           Craft.SSH


data PrivateKey
  = PrivateKey
    { _user    :: User
    , _name    :: String
    , _content :: String
    }
  deriving (Eq, Show)
makeLenses ''PrivateKey


path :: PrivateKey -> Craft AbsFilePath
path pk = do
  n <- parseRelFile $ pk^.name
  return $ (pk^.user.to userDir.Craft.path)</>n


instance Craftable PrivateKey PrivateKey where
  watchCraft pk = do
    craft_ $ userDir $ pk^.user
    pkPath <- Craft.SSH.PrivateKey.path pk
    w <-
      watchCraft_ $
        file pkPath
        & mode          .~ Mode RW O O
        & ownerAndGroup .~ pk ^. user
        & strContent    .~ pk ^. content
    return (w, pk)
