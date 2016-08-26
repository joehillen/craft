{-# LANGUAGE FlexibleInstances #-}
module Craft.Internal.Helpers where

import           Data.Char (isSpace)


indent :: Int -> String -> String
indent len text =
  unlines $ map (replicate len ' ' ++) $ lines text

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

class ToArg a where
  toArg :: String -> a -> [String]

instance ToArg String where
  toArg arg s
    | s == "" = []
    | otherwise = [arg, s]

instance ToArg Bool where
  toArg _   False = []
  toArg arg True  = [arg]

instance ToArg a => ToArg (Maybe a) where
  toArg _   Nothing  = []
  toArg arg (Just v) = toArg arg v

instance ToArg Int where
  toArg = showArg

toArgBool :: String -> String -> Bool -> [String]
toArgBool a _ True  = [a]
toArgBool _ b False = [b]

showArg :: Show a => String -> a -> [String]
showArg arg v = [arg, show v]

toArgs :: ToArg a => String -> [a] -> [String]
toArgs arg = Prelude.concatMap (toArg arg)
