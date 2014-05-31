import Data.List

wordNums :: String -> [(Int, String)]
wordNums = sort . map (\ws -> (length ws, head ws)) . group . sort . words
