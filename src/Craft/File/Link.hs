module Craft.File.Link where

import           Control.Lens

import           Craft


newtype TargetPath = TargetPath FilePath
  deriving (Show, Eq)

newtype LinkPath = LinkPath (Path Abs FileP)
  deriving (Show, Eq)

data Link
 = Link
   { _linkTarget :: TargetPath
   , _linkPath   :: LinkPath
   }
  deriving (Eq, Show)
makeLenses ''Link

-- TODO: intsance FileLike Link where


link :: TargetPath -> LinkPath -> Link
link = Link


exists :: Path Abs FileP -> Craft Bool
exists lp = isExecSucc <$> exec "test" ["-L", fromAbsFile lp]


readlink :: LinkPath -> Craft (Maybe TargetPath)
readlink (LinkPath lp) =
  exec "readlink" [fromAbsFile lp] >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> return . Just . TargetPath . trimTrailing $ r^.stdout


create :: Link -> Craft ()
create (Link t p) = justdoit t p
 where
  justdoit (TargetPath tp) (LinkPath lp) = exec_ "ln" ["-snf", tp, fromAbsFile lp]


instance Craftable Link Link where
  watchCraft l = do
    let lp = l ^. linkPath
    readlink lp >>= \case
      Nothing -> do
        craft_ l
        return (Created, l)
      Just target' ->
        if target' == l^.linkTarget then
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
        when (target' /= tp) $
          $craftError $ "craft `" ++ show l ++ "` failed! "
                     ++ "Wrong Target: " ++ show target' ++ " "
                     ++ "Expected: "    ++ show (l ^. linkTarget)
