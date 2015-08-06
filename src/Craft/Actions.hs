module Craft.Actions where

import           Control.Monad (void)
import           Data.Maybe (isJust)

import          Craft.Types

isPresent :: Craftable a => a -> Craft Bool
isPresent a = isJust <$> checker a

craft :: Craftable a => a -> Craft a
craft a = do
  crafter a
  checker a >>= \case
    Nothing -> error $ "craft failed for: " ++ show a
    Just  r -> return r

craft_ :: Craftable a => a -> Craft ()
craft_ = void . craft

remove :: Craftable a => a -> Craft a
remove a =
  checker a >>= \case
    Nothing -> return a
    Just  _ -> do
      remover a
      checker a >>= \case
        Nothing -> return a
        Just  r -> error $ "remove failed for: " ++ show a ++
                           " Found: " ++ show r

remove_ :: Craftable a => a -> Craft ()
remove_ = void . remove

