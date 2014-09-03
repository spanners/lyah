type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

unbalancedBirdsOnPole :: Maybe Pole
unbalancedBirdsOnPole = return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)

banana :: Pole -> Maybe Pole
banana _ = Nothing

bananaOnPole :: Maybe Pole
bananaOnPole = return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    banana first  
    second <- landRight 2 first  
    landLeft 1 second  
