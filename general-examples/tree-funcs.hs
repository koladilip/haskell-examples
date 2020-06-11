module Main where
import Data.List hiding (insert)
-- import Test.QuickCheck

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show


infinite_tree :: Tree (Integer, Integer)
infinite_tree = infinite_tree_gen (0, 0) 
    where 
        infinite_tree_gen (a, b) = Node (infinite_tree_gen (a+1, b)) (a, b) (infinite_tree_gen (a, b + 1))

cut :: Integer -> Tree a -> Tree a
cut 0 _ = Leaf 
cut n Leaf = Leaf
cut n (Node l p r ) = Node (cut (n-1) l) p (cut (n-1) r)

insert :: (Ord a) => a -> Tree a -> Tree a
insert a Leaf = Node (Leaf) a (Leaf)
insert a (Node l p r)
    | a < p = Node (insert a l) p r
    | otherwise = Node l p (insert a r)

insert_list :: (Ord a) => [a] -> Tree a
insert_list xs = foldr insert Leaf xs

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node l p r) = (inorder l) ++ [p] ++ (inorder r)

-- prop_IIS xs = sort xs === xs'
--     where
--         types = xs :: [Int]
--         xs' = inorder $ insert_list xs

main = do
    -- print $ cut 3 infinite_tree
    let t = insert_list [5, 6, 2, 1, 4, 7, 3]
    print $ inorder t
    