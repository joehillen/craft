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

changed :: WatchResult a -> Bool
changed (Unchanged _) = False
changed _             = True

created :: WatchResult a -> Bool
created (Created _) = True
created _           = False

updated :: WatchResult a -> Bool
updated (Updated _) = True
updated _           = False

removed :: WatchResult a -> Bool
removed (Removed _) = True
removed _           = False

unwatch :: WatchResult a -> a
unwatch (Unchanged a) = a
unwatch (Created a)   = a
unwatch (Updated a)   = a
unwatch (Removed a)   = a

watchCraft :: Craftable a => a -> Craft (WatchResult a)
watchCraft w = do
  !mb_before <- checker w
  !after <- craft w
  return $ case mb_before of
    Nothing     -> Created after
    Just before -> if before == after then Unchanged after
                                      else Updated after

watchRemove :: Craftable a => a -> Craft (WatchResult a)
watchRemove a =
  checker a >>= \case
    Nothing -> return $ Unchanged a
    Just before -> do
      remover a
      checker a >>= \case
        Nothing -> return $ Removed before
        Just  r -> error $ "remove failed for: " ++ show a ++
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

whenChanged :: WatchResult a -> (a -> Craft ()) -> Craft ()
whenChanged v f = when (changed v) $ f (unwatch v)
