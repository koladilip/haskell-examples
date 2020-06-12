module Grains (square, total) where

square' :: Integer -> Integer
square' n
    | n > 1 = 2 * square' (n-1)
    | otherwise = 1

square :: Integer -> Maybe Integer
square n
    | n > 64 || n < 1 = Nothing
    | otherwise = Just (square' n)


total :: Integer
-- 2^0 + 2^1 + 2^2 .. + 2^63 = 2^64 - 1 
total = (2 * square' 64) - 1
