module Craft.File.Link where

import Craft
import Control.Lens


newtype Target = Target FilePath
  deriving (Show, Eq)

newtype Path = Path FilePath
  deriving (Show, Eq)

data Link
 = Link
   { _target :: Target
   , _path   :: Path
   }
  deriving (Eq, Show)
makeLenses ''Link


link :: Target -> Path -> Link
link = Link


exists :: FilePath -> Craft Bool
exists lp = isExecSucc <$> exec "/usr/bin/test" ["-L", lp]


readlink :: Path -> Craft (Maybe FilePath)
readlink (Path lp) =
  exec "/bin/readlink" [lp] >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> return . Just . trimTrailing $ r ^. stdout


create :: Link -> Craft ()
create (Link t p) = justdoit t p
 where
  justdoit (Target tp) (Path lp) = exec_ "ln" ["-snf", tp, lp]


instance Craftable Link where
  watchCraft l = do
    let lp = l ^. path
    readlink lp >>= \case
      Nothing -> do
        craft_ l
        return (Created, l)
      Just target' ->
        if Target target' == l ^. target then
          return (Unchanged, l)
        else do
          craft_ l
          return (Updated, l)

  craft_ l = do
    let lp = l ^. path
    let tp = l ^. target
    create l
    readlink lp >>= \case
      Nothing -> $craftError $ "craft `" ++ show l ++ "` failed! Not Found."
      Just target' ->
        when (Target target' /= tp) $
          $craftError $ "craft `" ++ show l ++ "` failed! "
                     ++ "Wrong Target: " ++ target' ++ " "
                     ++ "Expected: "    ++ show (l ^. target)
