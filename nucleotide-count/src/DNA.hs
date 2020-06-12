module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

dnaMap :: Map Nucleotide Int
dnaMap = foldl (\acc c -> Map.insert c 0 acc) Map.empty [A, C, G, T]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldl addToMap (Right dnaMap) xs
    where 
        addToMap (Left a) _  = Left a
        addToMap (Right a) c
            | c == 'A' = Right (Map.insertWith (+) A 1 a)
            | c == 'C' = Right (Map.insertWith (+) C 1 a)
            | c == 'G' = Right (Map.insertWith (+) G 1 a)
            | c == 'T' = Right (Map.insertWith (+) T 1 a)
            | otherwise = Left xs
