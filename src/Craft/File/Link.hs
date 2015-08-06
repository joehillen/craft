module Craft.File.Link where

import           Craft
import qualified Craft.File as File

data Link
 = Link
   { target :: File.Path
   , path   :: File.Path
   }
  deriving (Eq, Show)

exists :: File.Path -> Craft Bool
exists lp = do
  (ec, _, _) <- exec "/usr/bin/test" ["-L", lp]
  return $ isSuccess ec

readLink :: File.Path -> Craft File.Path
readLink lp = do
  (ec, stdout, _) <- exec "/bin/readlink" [lp]
  if isSuccess ec then
    return $ trimTrailing stdout
  else
    error $ "readLink `" ++ lp ++ "` failed."

get :: File.Path -> Craft (Maybe Link)
get lp = do
  exists' <- exists lp
  if not exists' then
    return Nothing
  else do
    target' <- readLink lp
    return . Just $
      Link { target  = target'
           , path = lp
           }

instance Craftable Link where
  checker = get . path
  crafter Link{..} =
    exec_ "/bin/ln" ["-snf", target, path]
  remover = notImplemented "remover Link"

