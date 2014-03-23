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
