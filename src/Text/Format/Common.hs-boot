module Text.Format.Common where

import Text.ParserCombinators.ReadP
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

data Stripe string = Black string | White Blank

data Blank = Curly | CurlyInt Int | Square | SquareInt Int

parseBlank :: ReadP Blank

newtype Format string = Format { _zebra :: [Stripe string] }

format :: Format a -> Q Exp
