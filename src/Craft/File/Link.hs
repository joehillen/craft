module Craft.File.Link where

import Control.Lens

import Craft


newtype Target = Target FilePath
  deriving (Show, Eq)

newtype Path = Path FilePath
  deriving (Show, Eq)

data Link
 = Link
   { _linkTarget :: Target
   , _linkPath   :: Path
   }
  deriving (Eq, Show)
makeLenses ''Link

-- TODO: intsance FileLike Link where


link :: Target -> Path -> Link
link = Link


exists :: FilePath -> Craft Bool
exists lp = isExecSucc <$> exec "test" ["-L", lp]


readlink :: Path -> Craft (Maybe FilePath)
readlink (Path lp) =
  exec "readlink" [lp] >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> return . Just . trimTrailing $ r ^. stdout


create :: Link -> Craft ()
create (Link t p) = justdoit t p
 where
  justdoit (Target tp) (Path lp) = exec_ "ln" ["-snf", tp, lp]


instance Craftable Link Link where
  watchCraft l = do
    let lp = l ^. linkPath
    readlink lp >>= \case
      Nothing -> do
        craft_ l
        return (Created, l)
      Just target' ->
        if Target target' == l ^. linkTarget then
          return (Unchanged, l)
        else do
          craft_ l
          return (Updated, l)

  craft_ l = do
    let lp = l ^. linkPath
    let tp = l ^. linkTarget
    create l
    readlink lp >>= \case
      Nothing -> $craftError $ "craft `" ++ show l ++ "` failed! Not Found."
      Just target' ->
        when (Target target' /= tp) $
          $craftError $ "craft `" ++ show l ++ "` failed! "
                     ++ "Wrong Target: " ++ target' ++ " "
                     ++ "Expected: "    ++ show (l ^. linkTarget)
