import Data.Char

-- Uppercase a file

main = do
	contents <- getContents
	putStr (map toUpper contents)
