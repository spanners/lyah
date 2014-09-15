localMaxima :: [Integer] -> [Integer]
localMaxima = map piggy . maximas . spin3

spin3 :: [a] -> [(a, a, a)]
spin3 xs = zip3 xs (tail xs) (tail $ tail xs)

maximas :: Ord a => [(a, a, a)] -> [(a, a, a)]
maximas xs = filter (\(x, y, z) -> x < y && y > z) xs

piggy :: (a, b, c) -> b
piggy (x,y,z) = y 
