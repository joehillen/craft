module Craft.Actions where

import Control.Monad (void)
import Data.Maybe (isJust)

import Craft.Types

isPresent :: Craftable a => a -> Craft Bool
isPresent a = isJust <$> checker a

craft :: Craftable a => a -> Craft a
craft a = do
  mb <- checker a
  case mb of
    Nothing -> go mb
    Just r ->
      if r /= a then
        go mb
      else
        return r

 where
  go mb = do
    crafter a mb
    checker a >>= \case
      Nothing -> error $ "craft failed (not found) for: " ++ show a
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
        Just  r -> error $ "destroy failed for: " ++ show a ++
                           " Found: " ++ show r

destroy_ :: Craftable a => a -> Craft ()
destroy_ = void . destroy

