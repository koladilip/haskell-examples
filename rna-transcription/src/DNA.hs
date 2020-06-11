module DNA (toRNA) where

dnaToRNA :: Char -> Either Char Char
dnaToRNA 'G' = Right 'C'
dnaToRNA 'C' = Right 'G'
dnaToRNA 'T' = Right 'A'
dnaToRNA 'A' = Right 'U'
dnaToRNA c = Left c

toRNA :: String -> Either Char String
toRNA = mapM dnaToRNA