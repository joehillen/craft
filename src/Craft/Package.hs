module Craft.Package where

import Control.Lens
import Control.Monad.Reader

import Craft.Types


get :: PackageName -> Craft (Maybe Package)
get name = do
  pm <- asks _craftPackageManager
  (pm ^. pmGetter) name
