module Main where

elm :: (Eq a) => a -> [a] -> Bool
elm _ [] = False
elm a (x:xs) = (a == x) || (elm a xs)

nub :: (Eq a) => [a] -> [a]

nub [] = []
nub (x:xs)
    | elm x xs = nub xs
    | otherwise = (x : nub xs)

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) = (x <= y) && isAsc(y:xs)

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath xs x y 
    | (x == y ) = True
    | otherwise = or [hasPath xs' n y | (m, n) <- xs, m == x]
        where
            xs' = [(m, n) | (m, n) <- xs, m /= x] 

rev :: [a] ->[a]
rev xs = rev' xs []
    where 
        rev' [] ys = ys
        rev' (x:xs') ys = rev' xs' (x:ys)

-- Reverse using fold
revf :: [a] ->[a]
revf xs = foldl (\acc x -> x:acc) [] xs

-- Reverse using fold with flip
revff :: [a] ->[a]
revff = foldl (flip (:)) []

prefixes :: [a] -> [[a]]
prefixes l = rev $ map rev $ foldl customAdd [] l
    where
        customAdd [] y = [[y]]
        customAdd (x:xs) y = ((y:x):(x:xs))

prefixes2 :: [a] -> [[a]]
prefixes2 = foldr (\x acc -> [x] : map (x:) acc) []

lagrange :: [(Float, Float)] -> Float -> Float
lagrange xs a = foldl (\acc (x, y) -> acc + (y * (lbp x))) 0 xs
    where lbp x' = foldl (\acc (x, _) -> acc * (a - x) / (x' - x) ) 1 [(x,y) | (x,y) <- xs, x /= x']

data Trie a = Leaf a | Node a [Trie a]
foldtrie :: (b -> a -> b) -> b -> Trie a -> b 
foldtrie f acc (Leaf a) = f acc a
foldtrie f acc (Node a as) = foldl (foldtrie f) (f acc a) as

main = do
    print (elm 2 [1, 3, 2])
    print (nub [2, 1, 2, 3, 1, 4])
    print (isAsc [1, 2, 3]) 
    print (isAsc [1, 2, 3, 1]) 
    print (hasPath [(1, 2), (2, 3), (3, 4)] 1 4)
    print (hasPath [(1, 2), (2, 3), (3, 4)] 3 1)
    print (hasPath [(1, 2), (2, 3), (3, 4)] 2 4)
    print (rev [1, 2, 3])
    print (revf [1, 2, 3])
    print (revff [1, 2, 3])
    print $ prefixes [1, 2, 3]
    print $ prefixes2 [1, 2, 3]
    print $ lagrange [(1, 1), (2, 8), (3, 27), (4, 64)] 6
    print $ foldtrie (flip (:)) [] (Node 'c' [(Node 'a' [Leaf 'r', Leaf 't']),(Node 'o' [Node 'o' [Leaf 'l']])])

