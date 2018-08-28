{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Experiment
  where

import Data.Coerce

-- $setup
-- λ :set -XTypeApplications

class Microtemplate micro
  where
    (..?) :: String -> micro -> micro
    (?..) :: micro -> String -> micro
    infixl 2 ..?
    infixl 2 ?..

instance Microtemplate String
  where
    s ..? m = s ++ m
    m ?.. s = m ++ s

-- |
-- λ "One: " ..? show $ 1
-- "One: 1"

instance (Show show, Microtemplate micro) => Microtemplate (show -> micro)
  where
    s ..? m = \x -> s ..? m x
    m ?.. s = \x -> m x ?.. s

-- |
-- λ ("Two: " ..? (show .) . (+)) 1 1
-- "Two: 2"
-- λ ("Three: " ..? \x y z -> show $ x + y + z) 1 1 1
-- "Three: 3"

(...) :: (Microtemplate micro, Show show) => String -> micro -> show -> micro
s ... u = \x -> (s ++ show x) ..? u

infixr 3 ...

-- |
-- λ ("la" ... "la" ... "fa") 1 2
-- "la1la2fa"

class M ml mr mo
  where
    (-=-) :: ml -> mr -> mo

instance Microtemplate mr => M String mr mr
  where
    s -=- m = s ..? m

instance M ml mr mo => M (String -> ml) mr (String -> mo)
  where
    ml -=- mr = \s -> ml s -=- mr

-- |
-- λ putStrLn $ (((++) @Char) -=- ((++) @Char)) "Captain " "Pumpkin " "meows: " "Mek!"
-- Captain Pumpkin meows: Mek!

newtype F a r = F { apple :: a -> r }

instance Functor (F a)
  where
    fmap :: forall r r'. (r -> r') -> F a r -> F a r'
    fmap f = coerce (fmap f :: (a -> r) -> (a -> r'))

-- |
-- λ fmap show succ 1
-- "2"
-- λ apple (fmap show (F succ)) 1
-- "2"

newtype F' f a r = F' { apple' :: a -> f a r }

instance Functor (f a) => Functor (F' f a)
  where
    fmap :: forall r r'. (r -> r') -> F' f a r -> F' f a r'
    fmap f = coerce ((fmap . fmap) f :: (a -> f a r) -> (a -> f a r'))

-- |
-- λ apple' (fmap show (F' (+))) 1 2
-- "3"

class StringMap f g a b
  where
    sfmap :: (a -> b) -> f -> g

instance StringMap a b a b
  where
    sfmap f = f

instance StringMap sm sm' a a' => StringMap (x -> sm) (x -> sm') a a'
  where
    sfmap f sm = sfmap f . sm

-- |
-- λ sfmap @(Integer -> Integer) @(Integer -> String) @Integer (("One" ++) . show) succ 1
-- "One2"
-- λ sfmap @(Integer -> Integer -> Integer) @(Integer -> Integer -> String) @Integer (("One" ++) . show) (+) 1 2
-- "One3"
