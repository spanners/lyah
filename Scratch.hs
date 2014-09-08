removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [ c | c <- st, c `elem` ' ':['A'..'Z'] ]

oneSixSixFiveZero :: Int
oneSixSixFiveZero = sum $ takeWhile (<10000) $ filter odd $ map (^2) [1..]

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n:collatz (n `div` 2)
    | odd  n = n:collatz (n * 3 + 1)

howManyGreaterThanFifteen = length 
                              . filter (\x -> (length x) > 15) 
                              . map collatz
                              . enumFromTo 1

sum' :: (Num a) => [a] -> a
sum' = foldl (\acc x -> acc + x) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

checkThisOut = map ($ 3) [(4+), (10*), (^2), sqrt]  

-- N.B. If a pattern match fails in a lambda, a runtime error occurs, so be careful! 
-- GHCi> (\(Just x) -> x) Nothing
-- *** Exception: <interactive>:3:2-15: Non-exhaustive patterns in lambda
-- GHCi> head []
-- *** Exception: Prelude.head: empty list
