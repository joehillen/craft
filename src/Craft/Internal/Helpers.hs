{-# LANGUAGE FlexibleInstances #-}
module Craft.Internal.Helpers where

import Control.Monad (void)
import Data.Monoid ((<>))
import Data.Char (isSpace)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Text
import Data.Text as T
import TextShow


appendNL :: Text -> Text
appendNL s =
  s <> if not (T.null s) then "\n" else ""


end :: Parser ()
end = try (void eol) <|> eof


indent :: Int -> Text -> Text
indent len text =
  T.unlines $ (T.replicate len " " <>) <$> T.lines text


class ToArg a where
  toArg :: Text -> a -> [Text]

instance ToArg String where
  toArg arg "" = []
  toArg arg s  = [arg, T.pack s]

instance ToArg Text where
  toArg arg "" = []
  toArg arg s  = [arg, s]

instance ToArg Bool where
  toArg _   False = []
  toArg arg True  = [arg]

instance ToArg a => ToArg (Maybe a) where
  toArg _   Nothing  = []
  toArg arg (Just v) = toArg arg v

instance ToArg Int where
  toArg = showArg

toArgBool :: Text -> Text -> Bool -> [Text]
toArgBool a _ True  = [a]
toArgBool _ b False = [b]

showArg :: TextShow a => Text -> a -> [Text]
showArg arg v = [arg, showt v]

toArgs :: ToArg a => Text -> [a] -> [Text]
toArgs arg = Prelude.concatMap (toArg arg)
