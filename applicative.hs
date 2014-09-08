import Control.Applicative


justs = [Just 2, Just 5, Just 8]

-- Takes a list of applicatives, and
-- transforms it into an applicative with a list!
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])


main = do
	print $ [(+),(*)] <*> [1,2] <*> [3,4]
	-- = [1+3, 1+4, 2+3, 2+4, 1*3, 1*4, 2*3, 2*4]
	-- = [4,5,5,6,3,4,6,8]
	--
	print $ (+) <$> (+3) <*> (*100) $ 5
	-- = ((5+3) + (5*100))
	-- = 508
	--
	print $ (*) <$> [2,5,10] <*> [8,10,11]
	-- = [ x*y | x <- [2,5,10], y <- [8,10,11]] 
	-- = [16,20,22,40,50,55,80,100,110]
	print $ sequenceA justs 
