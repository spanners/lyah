import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
	let (firstCoin, newGen) = random gen
	    (secondCoin, newGen') = random newGen
	    (thirdCoin, newGen'') = random newGen'
	in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

main = do
	print $ threeCoins (mkStdGen 11)
	print (take 5 $ randoms' (mkStdGen 11) :: [Float])
