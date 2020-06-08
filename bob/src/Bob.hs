module Bob (responseFor) where
import qualified Data.Text as T

isLower :: T.Text -> Bool
isLower xs = (T.toLower xs) == xs

isUpcase :: T.Text -> Bool
isUpcase xs = not (isLower xs) && (T.toUpper xs) == xs

isNotEmpty :: T.Text -> Bool
isNotEmpty xs = xs /= T.empty

isQuestion :: T.Text -> Bool
isQuestion xs = isNotEmpty xs && T.last xs == '?'



responseForInternal :: T.Text -> String
responseForInternal xs 
    | isUpcase xs && isQuestion xs = "Calm down, I know what I'm doing!"
    | isUpcase xs = "Whoa, chill out!"
    | isQuestion xs = "Sure."
    | isNotEmpty xs = "Whatever."
    | otherwise = "Fine. Be that way!"
    
responseFor :: T.Text -> String
responseFor xs = responseForInternal (T.strip xs)
