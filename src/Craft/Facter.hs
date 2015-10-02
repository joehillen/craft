module Craft.Facter where

import           Craft

setup :: Craft ()
setup = craft_ $ package "facter"

fact :: String -> Craft String
fact f = do
  r <- exec "/usr/bin/facter" [f]
  case (exitcode r) of
    ExitSuccess   -> return $ rmTrailingNL (stdout r)
    ExitFailure _ -> error $
      "Failed to get fact `" ++ f ++ "`. "
      ++ "Facter failed with error: " ++ (stderr r)

rmTrailingNL :: String -> String
rmTrailingNL = reverse . dropWhile (=='\n') . reverse
