{-# LANGUAGE FlexibleInstances #-}

module Text.Format.String where

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadP.Util
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

import {-# SOURCE #-} Text.Format.Common as Common


instance Read (Stripe String) where
    readsPrec _ = (parseStripe $$)

-- | Parse a Stripe.
--
-- λ parseStripe $$ "whale"
-- [(Black "whale","")]
--
-- λ parseStripe $$ "[]actinia"
-- [(White Square,"actinia")]
--
-- λ parseStripe $$ "anchovy{}"
-- [(Black "anchovy","{}")]

parseStripe :: ReadP (Stripe String)
parseStripe =   (White <$> parseBlank)
            <++ (Black <$> manyTill get (choice [lookAhead parseBlank, lookAhead eof]))

instance Read (Format String) where
    readsPrec _ = (parseFormat $$)

-- | Parse a complete format string.
--
-- λ :{
--      putStr . unlines . concat $ fmap show . _zebra . fst <$>
--          parseFormat $$ "Sponge[]walrus{}plankton[1]oyster{2}."
-- :}
-- Black "Sponge"
-- White Square
-- Black "walrus"
-- White Curly
-- Black "plankton"
-- White (SquareInt 1)
-- Black "oyster"
-- White (CurlyInt 2)
-- Black "."
--
parseFormat :: ReadP (Format String)
parseFormat = Format <$> do
    manyTill parseStripe eof

format :: Format String -> Q Exp
format = Common.format
