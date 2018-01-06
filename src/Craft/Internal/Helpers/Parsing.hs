module Craft.Internal.Helpers.Parsing where

import           Control.Monad          (void)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char (eol)


end :: Parsec Void String ()
end = try (void $ lookAhead eol) <|> eof
