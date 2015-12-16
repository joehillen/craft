module Craft.Watched where

import             Craft.Types
import             Craft.Actions
import             Craft.Log


data Watched
  = Unchanged
  | Created
  | Updated
  | Removed
  deriving (Eq, Show)


changed :: Watched -> Bool
changed = not . unchanged


unchanged :: Watched -> Bool
unchanged Unchanged = True
unchanged _         = False


created :: Watched -> Bool
created Created = True
created _       = False


updated :: Watched -> Bool
updated Updated = True
updated _       = False


removed :: Watched -> Bool
removed Removed = True
removed _       = False


maybeWatch :: (Watched -> Bool) -> (Watched, a) -> Maybe a
maybeWatch f (w, a) | f w       = Just a
                    | otherwise = Nothing


watchCraft :: Craftable a => a -> Craft (Watched, a)
watchCraft x = do
  !mb_before <- checker x
  !after <- craftWithoutChecker x mb_before
  let w = case mb_before of
            Nothing     -> Created
            Just before -> if before == after then Unchanged
                                              else Updated
  return (w, after)

watchCraft_ :: Craftable a => a -> Craft Watched
watchCraft_ = (fmap fst) . watchCraft


watchDestroy :: Craftable a => a -> Craft (Watched, a)
watchDestroy a =
  checker a >>= \case
    Nothing -> return (Unchanged, a)
    Just before -> do
      destroyer a
      checker a >>= \case
        Nothing -> return (Removed, before)
        Just  r -> $craftError $ "destroy failed for: " ++ show a
                                 ++ " Found: " ++ show r


watch :: Eq b
      => (a -> Craft (Maybe b))   -- checker
      -> a                        -- the thing to watch
      -> Craft r                  -- action
      -> Craft ((Watched, a), r) -- result
watch watcher x action = do
  !mb_before <- watcher x
  !r <- action
  !mb_after  <- watcher x
  return (watchCompare mb_before mb_after x, r)


mapWatch :: Eq b
         => (a -> Craft (Maybe b))     -- checker
         -> [a]                        -- the things to watch
         -> Craft r                    -- action
         -> Craft ([(Watched, a)], r) -- result
mapWatch watcher ws action = do
  !mb_befores <- mapM watcher ws
  !r <- action
  !mb_afters  <- mapM watcher ws
  return (zipWith3 watchCompare mb_befores mb_afters ws, r)


watchCompare :: Eq a => Maybe a -> Maybe a -> b -> (Watched, b)
watchCompare       Nothing      Nothing = (,) Unchanged
watchCompare (Just      _)      Nothing = (,) Removed
watchCompare       Nothing (Just     _) = (,) Created
watchCompare (Just before) (Just after)
                      | before == after = (,) Unchanged
                      |       otherwise = (,) Updated
