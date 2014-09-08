{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
import qualified Data.Foldable as F
import Data.Monoid
import Data.List

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- The Functor typeclass is for things that can be mapped over.
-- Here we make Tree an instance of Functor, and tell Haskell how we want it to be mapped over.
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
    
instance F.Foldable Tree where  
    foldMap :: Monoid b => (a -> b) -> Tree a -> b 
    foldMap f EmptyTree = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             	f x        `mappend`  
                             	F.foldMap f r 

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

nums = [8,6,4,1,7,3,5]

numsTree = foldr treeInsert EmptyTree nums

sumNums = F.foldl (+) 0 numsTree
productNums = F.foldl (*) 1 numsTree
anyLittleNums = getAny $ F.foldMap (\x -> Any $ x < 2) numsTree
anyBigNums = getAny $ F.foldMap (\x -> Any $ x > 15) numsTree
listTree = F.foldMap (\x -> [x]) numsTree
theSame = listTree == sort nums
