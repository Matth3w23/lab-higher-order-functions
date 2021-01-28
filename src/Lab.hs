--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Higher-order functions                                                --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( elem, maximum, intersperse, transpose
                      , subsequences, permutations, any, all, flip, takeWhile
                      , zipWith, groupBy, notElem )
import Data.List (delete)

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

elem :: Eq a => a -> [a] -> Bool
--elem n [] = False
--elem n (x:xs) = (n == x) || elem n xs

--elem n = foldr (\x r -> (x==n) || r) False
--elem n xs = not (null (filter (== n) xs))

elem n = any ( == n)

maximum :: Ord a => [a] -> a --QUESTION, EXPLAIN WHY NOT NEED XS (maximum xs = foldr1 max xs)
maximum = foldr1 max

any :: (a -> Bool) -> [a] -> Bool
any func = foldr (\element recurResult -> func element || recurResult) False

all :: (a -> Bool) -> [a] -> Bool
all func = foldr (\element recurResult -> func element && recurResult) True

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile pred [] = []
takeWhile pred (x:xs)
    | pred x = x : takeWhile pred xs
    | otherwise = []
    

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith func (a:args) (e:effects) = func a e : zipWith func args effects

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy pred [] = []
groupBy pred (x:xs) = (x : takeWhile (pred x) xs) : groupBy pred (dropWhile (pred x) xs)


permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations list = [l:ls | l <- list, ls <- permutations (delete l list)]

--------------------------------------------------------------------------------
