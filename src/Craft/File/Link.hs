module Craft.File.Link where

import           Craft


data Link
 = Link
   { target :: FilePath
   , path   :: FilePath
   }
  deriving (Eq, Show)

exists :: FilePath -> Craft Bool
exists lp = isExecSucc <$> exec "/usr/bin/test" ["-L", lp]


readLink :: FilePath -> Craft FilePath
readLink lp =
  trimTrailing . stdout . errorOnFail <$> exec "/bin/readlink" [lp]

get :: FilePath -> Craft (Maybe Link)
get lp = do
  exists' <- exists lp
  if not exists' then
    return Nothing
  else do
    target' <- readLink lp
    return . Just
           $ Link { target  = target'
                  , path = lp
                  }

instance Craftable Link where
  checker = get . path
  crafter l ml = do
    let mkLink = exec_ "/bin/ln" ["-snf", (target l), (path l)]
    case ml of
      Nothing   -> mkLink
      Just oldl -> when (target oldl /= target l) $ mkLink

  destroyer = notImplemented "destroyer Link"

