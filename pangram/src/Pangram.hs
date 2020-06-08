module Pangram (isPangram) where
    
import Data.List as List
import qualified Data.Char as Char

alphaList :: String
alphaList = ['a'..'z']

isPangram :: String -> Bool
isPangram text =  List.intersect alphaList (map Char.toLower text)  == alphaList
