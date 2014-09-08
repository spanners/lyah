{-
newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show)  

instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x  

-- similarly for Last and getLast
-}

import Data.Monoid

mixedBag = [Nothing, Nothing, Nothing, Just 9, Just 10, Nothing]

justNine = (getFirst . mconcat . map First) mixedBag
justTen = (getLast . mconcat . map Last) mixedBag
