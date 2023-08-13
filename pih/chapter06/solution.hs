module Chapter06Solutions where
import Prelude hiding ((^), (!!))

-- 1.
-- Given a negative argument, the recursion does not end.
fac :: Int -> Int
fac 0 = 1
fac n | n > 0     = n * fac (n-1)
      | otherwise = m * fac (m - 1)
      where m = negate n

-- 2.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3.
-- 2 ^ 3
-- = { applying (^) }
-- 2 * (2 ^ 2)
-- = { applying (^) }
-- 2 * (2 * (2 ^ 1))
-- = { applying (^) }
-- 2 * (2 * (2 * (2 ^ 0)))
-- = { applying (^) }
-- 2 * (2 * (2 * 1))
-- = { applying (*) }
-- 8
--
-- Issue: When the operator ^ is used more than once,
-- the operator has a problem of being left associated.
-- For example, 2 ^ 2 ^ 3 = 64, not 256.
(^) :: Int -> Int -> Int
n ^ 0 = 1
n ^ m = n * (n ^ (m-1))

-- 4.
euclid :: Int -> Int -> Int
euclid n m | n == m    = n
           | n > m     = euclid (n-m) m
           | otherwise = euclid m n

-- 5.
-- length [1,2,3]
-- = { applying length }
-- 1 + length [2,3]
-- = { applying length }
-- 1 + (1 + length [3])
-- = { applying length }
-- 1 + (1 + (1 + length []))
-- = { applying length }
-- 1 + (1 + (1 + 0))
-- = { applying (+) }
-- 3
--
-- drop 3 [1,2,3,4,5]
-- = { applying drop }
-- drop 2 [2,3,4,5]
-- = { applying drop }
-- drop 1 [3,4,5]
-- = { applying drop }
-- drop 0 [4,5]
-- = { applying drop }
-- [4,5]
--
-- init [1,2,3]
-- = { applying init }
-- 1 : init [2,3]
-- = { applying init }
-- 1 : 2 : init [3]
-- = { applying init }
-- 1 : 2 : []
-- = { applying cons; list notation }
-- [1,2]

-- 6.a
and' :: [Bool] -> Bool
and' []                 = True
and' (b:bs) | b == True = and' bs
            | otherwise = False

-- 6.b
concat' :: [[a]] -> [a]
concat' []       = []
concat' [xs]     = xs
concat' (xs:xss) = xs ++ concat' xss

-- 6.c
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- 6.d
(!!) :: [a] -> Int -> a
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n-1)

-- 6.e
elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' x (y:ys) | x == y    = True
               | otherwise = elem' x ys

-- 7.
merge :: Ord a => [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 8.
halve :: [a] -> ([a],[a])
halve xs = (take size xs, drop size xs)
           where size = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort xs | length xs <= 1 = xs
         | otherwise      = merge (msort (fst (halve xs)))
                                  (msort (snd (halve xs)))

-- 9.a
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 9.b
take' :: Int -> [a] -> [a]
take' 0 xs = xs
take' _ [] = []
take' n (x:xs) = x : take (n-1) xs

-- 9.c
last' :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs

