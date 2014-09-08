import Test.QuickCheck

positives :: Gen Integer
positives = 
 do -- Pick an arbitrary integer:
    x <- arbitrary 
    -- Make it positive, if necessary:
    if (x == 0) 
      then return 1
    else if (x < 0)
      then return (-x)
    else 
      return x
	  
f :: Integer -> Integer
f(n) | even(n) = n `div` 2
     | odd(n)  = 3*n + 1

collatz :: Integer -> Bool
collatz(1) = True
collatz(n) = collatz(f(n))