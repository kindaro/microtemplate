{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Experiment
  where

class Microtemplate micro
  where
    (?) :: String -> micro -> micro
    infixl 2 ?

instance {-# overlaps #-} Show show => Microtemplate (show -> String)
  where
    s ? m = \x -> s ++ m x

-- |
-- λ "One: " ? show $ 1
-- "One: 1"

instance (Show show, Microtemplate micro) => Microtemplate (show -> micro)
  where
    s ? m = \x -> s ? m x

-- |
-- λ ("Two: " ? (show .) . (+)) 1 1
-- "Two: 2"
-- λ ("Three: " ? \x y z -> show $ x + y + z) 1 1 1
-- "Three: 3"

class Ellipsis left right out
  where
    (...) :: left -> right -> out
    infixl 9 ...

instance Show show => Ellipsis String String (show -> String)
  where
    s ... t = \x -> s ++ show x ++ t
