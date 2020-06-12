module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = (squareOfSum n) - (sumOfSquares n)

square :: Integral a => a -> a
square n = n * n

squareOfSum :: Integral a => a -> a
squareOfSum n = square (div (n * (n+1)) 2) ;

sumOfSquares :: Integral a => a -> a
sumOfSquares n = div (n * ((n+1) * ((2*n) + 1)))  6
