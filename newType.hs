newtype Pair a = MkPair { getPair :: (a, a) } deriving (Eq)

instance Functor Pair where 
	fmap f (MkPair (a, b)) = MkPair (f a, f b)

instance Show a => Show (Pair a) where
	show = show . getPair
