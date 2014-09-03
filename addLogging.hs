import Control.Monad.Writer

-- your everyday GCD function
gcd' :: Int -> Int -> Int  
gcd' a b   
    | b == 0    = a  
    | otherwise = gcd' b (a `mod` b)  

-- the same program, with logging
gcd'' :: Int -> Int -> Writer [String] Int  
gcd'' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd'' b (a `mod` b) 

gcd''Logs = mapM_ putStrLn $ snd $ runWriter (gcd'' 8 3)


-- this is useful for making append-heavy functiong, such as those with logging
import Data.DList


-- Here, we prepend to a DList instead of appending to a normal haskell list, for great efficiency!
gcd''' :: Int -> Int -> Writer (DList String) Int  
gcd''' a b  
    | b == 0 = do  
        tell $ fromList $ ["Finished with " ++ show a]
        return a  
    | otherwise = do  
	-- this bit would be VERY slow with normal lists
        result <- gcd''' b (a `mod` b)  
	-- but because we are building the DList up by prepending not appending, it's okay to recur accumulating
        tell $ fromList $ [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result 

-- we convert back to a normal list for pretty printing
gcd'''Logs = mapM_ putStrLn $ toList . snd . runWriter $ gcd''' 8 3
