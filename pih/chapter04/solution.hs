module Chapter04Solutions where
import Prelude hiding ((||), (&&))

-- 1.
halve :: [a] -> ([a], [a])
halve xs = (take len xs, drop len xs)
           where len = length xs `div` 2

-- 2.a
third :: [a] -> a
third xs = head (tail (tail xs))

-- 2.b
third' :: [a] -> a
third' xs = xs !! 2

-- 2.c
third'' :: [a] -> a
third'' (_:_:x:xs) = x

-- 3.a
safetail :: [a] -> [a]
safetail xs = if length xs == 0 then xs
              else tail xs

-- 3.b
safetail' :: [a] -> [a]
safetail' xs | length xs == 0 = []
             | otherwise      = tail xs

-- 3.c
safetail'' :: [a] -> [a]
safetail'' []     = []
safetail'' (x:xs) = xs

-- 4.
(||) :: Bool -> Bool -> Bool
True || True   = True
True || False  = True
False || True  = True
False || False = False

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True 

(||||) :: Bool -> Bool -> Bool
False |||| b = b
True  |||| _ = True

(|||||) :: Bool -> Bool -> Bool
b1 ||||| b2 | b1 == b2  = b1
            | otherwise = True

-- 5.
(&&) :: Bool -> Bool -> Bool
b1 && b2 = if b1 == True then
             if b2 == True then True
             else False
           else False

-- 6.
(&&&) :: Bool -> Bool -> Bool
b1 &&& b2 = if b1 == True then b2
            else False

-- 7.
mult :: Int -> (Int -> (Int -> Int))
mult = (\x -> (\y -> (\z -> x * y * z)))

-- 8.
luhnDouble :: Int -> Int
luhnDouble x | doubled > 9 = doubled - 9
             | otherwise   = doubled
             where doubled = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = (sum [luhnDouble x, y, luhnDouble z, w]) `mod` 10 == 0

