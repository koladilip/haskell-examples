module Main where

import System.IO
import Data.List
import Data.Char


getUserLines :: [String] -> IO [String]                      -- optional type signature
getUserLines lines =  do
    line <- getLine
    if null line
        then return (reverse lines)
        else getUserLines (line:lines)

printSearchResult :: (String, Maybe Int) -> IO ()
printSearchResult (a, Just _) = do
    print $ a <> " is found"

printSearchResult (a, Nothing) = do
    print $ a <> " is not found"


split :: [Char] -> [Char] -> [[Char]]
split [] _  = []
split s d = filter (not.null) $ foldr (\c (w:ws) -> if (elemIndex c d) == Nothing then (((toLower c) : w) : ws) else ([]:(w:ws))) [[]] s

search inputWords fileWords = sortBy compareSearchResults $ map (\(word, lword) -> (word, elemIndex lword fileWords)) inputWordsWithLowerCase
        where inputWordsWithLowerCase = map (\word -> (word, map toLower word)) inputWords

compareSearchResults (_, Just _) (_, Just _) = EQ
compareSearchResults (_, Nothing) (_, Nothing)= EQ
compareSearchResults (_, Nothing) (_, Just _) = GT
compareSearchResults (_, Just _) (_, Nothing) = LT

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    print $ "Enter path of the file:"
    filePath <- getLine
    print $ "Enter words to search:"
    inputWords <- getUserLines []
    contents <- readFile filePath
    let fileWords = split contents ", ?;.\t\n\r"
    let searchResults = search inputWords fileWords 
    mapM_ (\r -> printSearchResult r) searchResults
