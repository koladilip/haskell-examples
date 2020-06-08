module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = rem year 400 == 0 || (rem year 100 /= 0 && rem year 4 == 0)
