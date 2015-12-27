module Craft.Facter where

import           Craft
import Control.Lens


setup :: Craft ()
setup = craft_ $ package "facter"


fact :: String -> Craft String
fact f = do
  r <- $errorOnFail =<< exec "/usr/bin/facter" [f]
  return $ r ^. stdout . to rmTrailingNL


rmTrailingNL :: String -> String
rmTrailingNL = reverse . dropWhile (=='\n') . reverse
