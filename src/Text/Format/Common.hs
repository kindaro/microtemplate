{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Format.Common where

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadP.Util
import Data.String
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

import qualified Text.Format.String ()

data Stripe string = Black string | White Blank deriving Show

data Blank = Curly | CurlyInt Int | Square | SquareInt Int deriving Show

instance Read Blank where
    readsPrec _ = (parseBlank $$)

-- | Parse a Blank.
--
-- λ parseBlank $$ "[]actinia"
-- [(Square,"actinia")]
--
-- λ parseBlank $$ "{}anchovy"
-- [(Curly,"anchovy")]
--
-- λ parseBlank $$ "[1]squid"
-- [(SquareInt 1,"squid")]
--
-- λ parseBlank $$ "{0xff}octopus"
-- [(CurlyInt 255,"octopus")]
--
-- Whitespace may be arbitrarily positioned inside a Blank.
--
-- λ parseBlank $$ "[ \t ]cuttlefish"
-- [(Square,"cuttlefish")]
--
-- λ parseBlank $$ "{\n}dolphin"
-- [(Curly,"dolphin")]
--
-- λ parseBlank $$ "[3   ]jellyfish"
-- [(SquareInt 3,"jellyfish")]
--
-- λ parseBlank $$ "{  4 }urchin"
-- [(CurlyInt 4,"urchin")]
--
-- Non-whitespace, beside an optional single number, does not parse.
--
-- λ parseBlank $$ "[shark 1]crab"
-- []
--
-- λ parseBlank $$ "{\NUL}eel"
-- []

parseBlank :: ReadP Blank
parseBlank = choice
        [ Curly     <$  between (char '{') (char '}') skipSpaces
        , CurlyInt  <$> between (char '{') (char '}') (inSpaces (readS_to_P reads :: ReadP Int))
        , Square    <$  between (char '[') (char ']') skipSpaces
        , SquareInt <$> between (char '[') (char ']') (inSpaces (readS_to_P reads :: ReadP Int))
        ]
  where
    inSpaces = between skipSpaces skipSpaces

newtype Format string = Format { _zebra :: [Stripe string] } deriving Show

-- | Format strings may be given simply as a string literal.
--
-- λ :set -XOverloadedStrings
-- λ "seal{}nautilus[]lobster" :: Format String
-- Format {_zebra = [Black "seal",White Curly,Black "nautilus",White Square,Black "lobster"]}
--
instance Read (Format a) => IsString (Format a) where
    fromString = read

format :: Format string -> Q Exp
format (Format xs') = do
    let xs = zipWith enumerate xs' [1..]
        len = length xs
    undefined
  where
    enumerate :: Stripe string -> Int -> Stripe string
    enumerate s i = case s of
        White Curly  -> White $ CurlyInt i
        White Square -> White $ SquareInt i
        x -> x

-- | Generate non-empty strings of length within [1..n].
--
-- λ names 2 ""
-- ["a","aa","ba"..."za","b","ab"..."xz","yz","zz"]
--
nonEmptyStrings :: Int -> String -> [String]
nonEmptyStrings 0 s = return s
nonEmptyStrings n s = undefined
  where
    iterate :: String -> [String]
    iterate s = [ c: s | c <- ['a'..'z'] ]
