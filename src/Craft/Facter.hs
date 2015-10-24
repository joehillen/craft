module Craft.Facter where

import           Craft


setup :: Craft ()
setup = craft_ $ package "facter"


fact :: String -> Craft String
fact f = do
  r <- errorOnFail <$> exec "/usr/bin/facter" [f]
  return $ rmTrailingNL (stdout r)


rmTrailingNL :: String -> String
rmTrailingNL = reverse . dropWhile (=='\n') . reverse
