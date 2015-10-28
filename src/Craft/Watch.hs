{-# LANGUAGE DeriveFunctor #-}
module Craft.Watch where

import             Control.Monad (when)
import             Craft.Types
import             Craft.Actions

data WatchResult a
  = Unchanged a
  | Created a
  | Updated a
  | Removed a
  deriving (Show, Functor)

changed :: WatchResult a -> Maybe a
changed (Unchanged _) = Nothing
changed (Created a)   = Just a
changed (Updated a)   = Just a
changed (Removed a)   = Just a

created :: WatchResult a -> Maybe a
created (Created a) = Just a
created _           = Nothing

updated :: WatchResult a -> Maybe a
updated (Updated a) = Just a
updated _           = Nothing

removed :: WatchResult a -> Maybe a
removed (Removed a) = Just a
removed _           = Nothing

unwatch :: WatchResult a -> a
unwatch (Unchanged a) = a
unwatch (Created a)   = a
unwatch (Updated a)   = a
unwatch (Removed a)   = a

watchCraft :: Craftable a => a -> Craft (WatchResult a)
watchCraft w = do
  !mb_before <- checker w
  !after <- craftWithoutChecker w mb_before
  return $ case mb_before of
    Nothing     -> Created after
    Just before -> if before == after then Unchanged after
                                      else Updated after

watchDestroy :: Craftable a => a -> Craft (WatchResult a)
watchDestroy a =
  checker a >>= \case
    Nothing -> return $ Unchanged a
    Just before -> do
      destroyer a
      checker a >>= \case
        Nothing -> return $ Removed before
        Just  r -> error $ "destroy failed for: " ++ show a ++
                           " Found: " ++ show r

watch :: Eq b
      => (a -> Craft (Maybe b))   -- checker
      -> a                        -- the thing to watch
      -> Craft r                  -- action
      -> Craft (WatchResult a, r) -- result
watch watcher w action = do
  !mb_before <- watcher w
  !r <- action
  !mb_after  <- watcher w
  return (watchCompare mb_before mb_after w, r)

mapWatch :: Eq b
         => (a -> Craft (Maybe b))     -- checker
         -> [a]                        -- the thing to watch
         -> Craft r                    -- action
         -> Craft ([WatchResult a], r) -- result
mapWatch watcher ws action = do
  !mb_befores <- mapM watcher ws
  !r <- action
  !mb_afters  <- mapM watcher ws
  return (zipWith3 watchCompare mb_befores mb_afters ws, r)

watchCompare :: Eq a => Maybe a -> Maybe a -> b -> WatchResult b
watchCompare       Nothing      Nothing = Unchanged
watchCompare (Just      _)      Nothing = Removed
watchCompare       Nothing (Just     _) = Created
watchCompare (Just before) (Just after)
                      | before == after = Unchanged
                      |       otherwise = Updated
