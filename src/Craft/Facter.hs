module Craft.Facter where

import           Craft

setup :: Craft ()
setup = craft_ $ package "facter"

fact :: String -> Craft String
fact f = do
  (exitcode, stdout, stderr) <- exec "/usr/bin/facter" [f]
  case exitcode of
    ExitSuccess   -> return $ rmTrailingNL stdout
    ExitFailure _ -> error $
      "Failed to get fact `" ++ f ++ "`. "
      ++ "Facter failed with error: " ++ stderr

rmTrailingNL :: String -> String
rmTrailingNL = reverse . dropWhile (=='\n') . reverse
