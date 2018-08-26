{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Experiment
  where

class Microtemplate micro

instance Show show => Microtemplate (show -> String)

instance (Show show, Microtemplate micro) => Microtemplate (show -> micro)

class Ellipsis left right out
  where
    (...) :: left -> right -> out
    infixl 9 ...

instance Show show => Ellipsis String String (show -> String)
  where
    s ... t = \x -> s ++ show x ++ t
