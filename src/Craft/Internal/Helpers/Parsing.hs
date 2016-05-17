module Craft.Internal.Helpers.Parsing where

import Control.Monad (void)

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.String


end :: Parser ()
end = try (void $ lookAhead eol) <|> eof
