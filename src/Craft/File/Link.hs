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
exists lp = isSuccess . exitcode <$> exec "/usr/bin/test" ["-L", lp]


readLink :: File.Path -> Craft File.Path
readLink lp = do
  r <- exec "/bin/readlink" [lp]
  if isSuccess (exitcode r) then
    return . trimTrailing $ stdout r
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

