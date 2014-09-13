{-# LANGUAGE InstanceSigs #-}

import Data.Ratio
import Data.List (all)

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap :: (a -> b) -> Prob a -> Prob b
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

someProb :: Prob Int
someProb = (Prob [(3,1%2),(5,1%4),(9,1%4)])

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)
    ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x,r) -> (x,p*r)) innerxs

instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

{-
All three of them will land tails nine times out of forty, which is
less than 25%. We see that our monad doesn't know how to join all of
the False outcomes where all coins don't land tails into one
outcome. That's not a big problem, since writing a function to put
all the same outcomes into one outcome is pretty easy and is left as
an exercise to the reader (you!)
-}

-- TODO: implement instance Monoid Prob,

