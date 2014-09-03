-- the list monad omg

import Control.Monad

sevens = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x  
