module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = multiples (limit-1) 
    where 
        factors' = [f | f <- factors, f>0]
        anyFactors n  = any (\f -> (rem n f) == 0) factors'
        multiples l
            | l>0 && anyFactors l = l + multiples (l-1)
            | l>0 = multiples (l-1)
            | otherwise = 0

