import System.Random  
  
main = do     
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    putStr $ take 20 (randomRs ('a','z') gen')
