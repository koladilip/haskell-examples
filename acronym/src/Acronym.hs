module Acronym (abbreviate) where
import Data.Char

abbreviate :: String -> String
abbreviate xs = toUpper (snd acronymLetters) : fst acronymLetters
    where 
        makeAcronym c (l, prevChar)
            | isUpper prevChar && not (isUpper c) = (prevChar : l, c)
            | isAlpha prevChar && not (isAlpha c) && c /= '\'' = (toUpper prevChar : l, c)
            | otherwise = (l, c)
        acronymLetters  = foldr makeAcronym ([], ' ') xs


