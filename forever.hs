import Control.Monad
import Data.Char

-- short

main = forever $ do
	putStr "Give me some input: "
	l <- getLine
	putStrLn $ map toUpper l
