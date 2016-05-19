module Craft.Facter where

import Craft


setup :: Craft ()
setup = craft_ $ package "facter"


fact :: String -> Craft String
fact f = rmTrailingNL <$> ($stdoutOrError =<< exec "facter" [f])


rmTrailingNL :: String -> String
rmTrailingNL = reverse . dropWhile (=='\n') . reverse
