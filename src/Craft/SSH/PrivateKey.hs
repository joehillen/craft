{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 711
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
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


path :: Getter PrivateKey FilePath
path = to $ \pk -> (pk^.user.to userDir.Craft.path)</>(pk^.name)


instance Craftable PrivateKey PrivateKey where
  watchCraft pk = do
    craft_ $ userDir $ pk^.user
    w <- watchCraft_ $ file (pk^.Craft.SSH.PrivateKey.path)
                       & mode          .~ Mode RW O O
                       & ownerAndGroup .~ pk ^. user
                       & strContent    .~ pk ^. content
    return (w, pk)
