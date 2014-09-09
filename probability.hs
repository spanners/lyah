{-# LANGUAGE InstanceSigs #-}

import Data.Ratio

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap :: (a -> b) -> Prob a -> Prob b
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

someProb = (Prob [(3,1%2),(5,1%4),(9,1%4)])

flatten :: Prob (Prob a) -> Prob a
flatten (Prob a) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x,r) -> (x,p*r)) innerxs
