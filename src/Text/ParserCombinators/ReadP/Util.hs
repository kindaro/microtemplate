module Text.ParserCombinators.ReadP.Util where

import Text.ParserCombinators.ReadP
import Control.Monad

-- | Return a parser that consumes nothing in case the argument parser would have succeeded, and
-- fails otherwise.

lookAhead :: ReadP a -> ReadP ()
lookAhead p = do
    s <- look
    when (null $ p $$ s) pfail

($$) :: ReadP a -> String -> [(a, String)]
parser $$ string = readP_to_S parser string
