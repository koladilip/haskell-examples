module PerfectNumbers (classify,  Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors :: Int -> [Int]
factors n = [f | f <- [1..(div n 2)], (rem n f) == 0 ]

classify :: Int -> Maybe Classification
classify n
    | n < 1 = Nothing
    | n == aliquotSum = Just Perfect
    | n > aliquotSum = Just Deficient
    | otherwise = Just Abundant
    where 
        aliquotSum = sum $ factors n 
