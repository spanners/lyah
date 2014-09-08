import Data.List

data Section = Section { getA :: Int
                       , getB :: Int
                       , getC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section  5 90 20
                   , Section 40  2  0 ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)
roadStep (pathA, pathB, priceA, priceB) (Section a b c) =
	let forwardPriceToA = priceA + a
	    crossPriceToA = priceB + b + c
	    forwardPriceToB = priceB + b
	    crossPriceToB = priceA + a + c
	    newPathToA = if forwardPriceToA <= crossPriceToA
			    then ((A,a):pathA, forwardPriceToA)
			    else ((C,c):(B,b):pathB, crossPriceToA)
            newPathToB = if forwardPriceToB <= crossPriceToB
			    then ((B,b):pathB, forwardPriceToB)
			    else ((C,c):(A,a):pathA, crossPriceToB)
	in (fst newPathToA, fst newPathToB, snd newPathToA, snd newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
	let (bestAPath, bestBPath, _, _) = bestPaths roadSystem 
            bestPath | totalCost bestAPath <= totalCost bestBPath = bestAPath
                     | otherwise                                  = bestBPath
	in reverse bestPath

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

totalCost :: Path -> Int
totalCost = sum . map snd

bestPaths :: RoadSystem -> (Path, Path, Int, Int)
bestPaths = foldl roadStep ([], [], 0, 0)

main = do
	contents <- getContents
	let threes = groupsOf 3 (map read $ lines contents)
            roadSystem = map (\[a,b,c] -> Section a b c) threes
	    path = optimalPath roadSystem
	    pathString = concat $ map (show . fst) path
            pathPrice = totalCost path
	putStrLn $ "The best path to take is: " ++ pathString
	putStrLn $ "The price is: " ++ show pathPrice
