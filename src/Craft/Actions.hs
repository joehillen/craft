module Craft.Actions where

import Control.Monad (void)
import Data.Maybe (isJust)

import Craft.Types

isPresent :: Craftable a => a -> Craft Bool
isPresent a = isJust <$> checker a

craft :: Craftable a => a -> Craft a
craft a =
  checker a >>= \case
    Just r  -> return r
    Nothing -> do
      crafter a
      checker a >>= \case
        Nothing -> error $ "craft failed for: " ++ show a
        Just  r -> return r

craft_ :: Craftable a => a -> Craft ()
craft_ = void . craft

destroy :: Craftable a => a -> Craft a
destroy a =
  checker a >>= \case
    Nothing -> return a
    Just  _ -> do
      destroyer a
      checker a >>= \case
        Nothing -> return a
        Just  r -> error $ "remove failed for: " ++ show a ++
                           " Found: " ++ show r

destroy_ :: Craftable a => a -> Craft ()
destroy_ = void . destroy

