module CollatzConjecture (collatz) where

collatz_internal :: Integer -> Integer
collatz_internal num 
    | num == 1 =  0
    | isEven = 1 + collatz_internal (num `div` 2)
    | otherwise = 1 + collatz_internal (3 * num + 1)
    where
        isEven = rem num 2 == 0

collatz :: Integer -> Maybe Integer
collatz num
    | num > 0 =  Just (collatz_internal num)
    | otherwise = Nothing
   
