module Craft.Facter where

import           Craft
import Control.Lens


setup :: Craft ()
setup = craft_ $ package "facter"


fact :: String -> Craft String
fact f = rmTrailingNL <$> ($stdoutOrError =<< exec "/usr/bin/facter" [f])


rmTrailingNL :: String -> String
rmTrailingNL = reverse . dropWhile (=='\n') . reverse
