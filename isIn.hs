-- This is just isInfixOf

import Data.List (isPrefixOf, tails)

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)
