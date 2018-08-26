{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Experiment
  where

class Microtemplate micro
  where
    (?..) :: String -> micro -> micro
    (..?) :: micro -> String -> micro
    infixl 2 ?..
    infixl 2 ..?

instance {-# overlaps #-} Show show => Microtemplate (show -> String)
  where
    s ?.. m = \x -> s ++ m x
    m ..? s = \x -> m x ++ s

-- |
-- λ "One: " ?.. show $ 1
-- "One: 1"

instance (Show show, Microtemplate micro) => Microtemplate (show -> micro)
  where
    s ?.. m = \x -> s ?.. m x
    m ..? s = \x -> m x ..? s

-- |
-- λ ("Two: " ?.. (show .) . (+)) 1 1
-- "Two: 2"
-- λ ("Three: " ?.. \x y z -> show $ x + y + z) 1 1 1
-- "Three: 3"

class Ellipsis left right out
  where
    (...) :: left -> right -> out
    infixr 9 ...

instance {-# overlaps #-} Show show => Ellipsis String String (show -> String)
  where
    s ... t = \x -> s ++ show x ++ t

instance ( Show show
         , Microtemplate micro
         ) => Ellipsis String micro (show -> micro)
  where
    s ... m = \x -> s ++ show x ?.. m

-- |
-- λ ("la" ... ("la" ... "fa" :: Int -> String)) (1 :: Int) (2 :: Int) :: String
-- "la1la2fa"
