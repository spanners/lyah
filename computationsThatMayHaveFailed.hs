-- This is useful when dealing with the results of computations that may have failed

import Data.Monoid

computations = mconcat [Just (Sum 3), Just (Sum 4), Nothing] 
