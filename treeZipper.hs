import Data.Maybe

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

coolTree = Node 1 (Node 4 (Node 2 Empty Empty) Empty) (Node 3 Empty Empty)
result1 = return (coolTree,[]) >>= goRight
result2 = return (coolTree,[]) >>= goRight >>= goRight
result3 = return (coolTree,[]) >>= goRight >>= goRight >>= goRight

modify :: (a -> a) -> Zipper a -> Maybe (Zipper a)
modify f (Node x l r, bs) = Just (Node (f x) l r, bs)
modify f (Empty, bs) = Just (Empty, bs)

-- newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')

-- newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')

attach :: Tree a -> Zipper a -> Maybe (Zipper a)
attach t (_, bs) = Just (t, bs)

farLeft = return (coolTree,[]) >>= goLeft >>= goLeft >>= goLeft

newFocus = return (coolTree,snd $ fromJust farLeft) >>= attach (Node 9 Empty Empty)

-- topMost :: Zipper a -> Zipper a
-- topMost (t,[]) = (t,[])
-- topMost z = topMost (goUp z)
