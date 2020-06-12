module ArmstrongNumbers (armstrong) where

digits :: Integral a => a -> [a]
digits n
    | n < 10 = [n]
    | otherwise = (rem n 10) : digits (div n 10)

armstrong :: Integral a => a -> Bool
armstrong n 
    | n < 10 = True
    | otherwise = (sum $ map (^l) digs) == n
        where digs = digits n; l = length digs
