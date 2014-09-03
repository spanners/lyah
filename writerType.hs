import Control.Monad.Writer  
  
logNumber :: Int -> Writer [String] Int  
logNumber x = tell ["Got number: " ++ show x] >> return x 
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)  

result = show $ runWriter multWithLog
