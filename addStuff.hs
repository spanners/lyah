import Control.Monad.Instances

addStuff :: Int -> Int
addStuff = do
	a <- (*2)
	b <- (+10)
	return (a+b)

main = do
	n <- getLine
	-- ghci> addStuff 3
	-- 19
	-- What's going on here?
	-- 3*2 = 6
	-- 3+10 = 13
	-- 6+13 = 19
	print $ addStuff (read n :: Int)
