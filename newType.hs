newtype Pair b a = MkPair { getPair :: (a, b) } deriving (Eq)

instance Functor (Pair c) where 
	fmap f (MkPair (a, b)) = MkPair (f a, b)

instance (Show a, Show b) => Show (Pair a b) where
	show = show . getPair

first (MkPair (x, _)) = x

{- 
Types defined with `newtype` are *lazier* than those defined with `data`.
This allows us to use `undefined` in their type constructors.
Haskell does not complain about `undefined` not existing, because it is evaluated lazily.
`first` pattern matches against (MkPair (x, _)),
it does not care about what is in the second position of the pair because
there can only be ONE type constructor for the type `Pair`.
In the case of a `Pair` defined with `data`, Haskell would complain, as it needs to
evaluate more of the expression to find out which type constructor (could be more than one)
is being used for the type `Pair`.
-}
foo = first . fmap (+2) $ MkPair (5, undefined)
