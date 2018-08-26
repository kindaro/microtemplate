{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.Format.String

main = print $ $(format "a{}") 1
