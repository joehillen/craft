module Craft.File.Link where

import Craft
import Control.Lens


data Link
 = Link
   { target :: FilePath
   , path   :: FilePath
   }
  deriving (Eq, Show)


exists :: FilePath -> Craft Bool
exists lp = isExecSucc <$> exec "/usr/bin/test" ["-L", lp]


readlink :: FilePath -> Craft (Maybe FilePath)
readlink lp =
  exec "/bin/readlink" [lp] >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> return . Just . trimTrailing $ r ^. stdout


instance Craftable Link where
  watchCraft l = do
    let lp = path l
    readlink lp >>= \case
      Nothing -> do
        craft_ l
        return (Created, l)
      Just target' ->
        if target' == target l then
          return (Unchanged, l)
        else do
          craft_ l
          return (Updated, l)

  craft_ l = do
    let lp = path l
    exec_ "ln" ["-snf", (target l), (path l)]
    readlink lp >>= \case
      Nothing -> error $ "craft Link `" ++ lp ++ "` failed! Not Found."
      Just target' ->
        when (target' /= target l) $
          error $ "craft Link `" ++ lp ++ "` failed! Wrong Target: " ++ target'
                                                 ++" Expected: " ++ target l
