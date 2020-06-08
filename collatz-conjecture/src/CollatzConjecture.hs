module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz num
    | num > 0 =  Just (collatz_internal num)
    | otherwise = Nothing
    where 
        collatz_internal pnum 
            | pnum == 1 =  0
            | even pnum = 1 + collatz_internal (pnum `div` 2)
            | otherwise = 1 + collatz_internal (3 * pnum + 1)
   
