module Craft.Facter where

import           Craft
import Control.Lens


setup :: Craft ()
setup = craft_ $ package "facter"


fact :: Text -> Craft Text
fact f = do
  r <- $errorOnFail =<< exec "/usr/bin/facter" [f]
  return $ r ^. stdout . to rmTrailingNL


rmTrailingNL :: Text -> Text
rmTrailingNL = reverse . dropWhile (=='\n') . reverse
