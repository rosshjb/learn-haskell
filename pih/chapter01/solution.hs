module Chapter01Solutions where
import Prelude hiding (product)

-- 1.
-- double (double 2)
-- = { applying inner double }
-- double (2 + 2)
-- = { applying double }
-- (2 + 2) + (2 + 2)
-- = { applying + }
-- 8

-- 2.
-- sum [x]
-- = { applying sum }
-- x + sum []
-- = { applying sum }
-- x + 0
-- = { applying + }
-- x

-- 3.
product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

-- 4.
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                 larger  = [a | a <- xs, a >= x]
                 smaller = [b | b <- xs, b < x] 

-- 5: The same values are not included in the result.
qsort' :: Ord a => [a] -> [a]
qsort' []     = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
                where
                  smaller = [a | a <- xs, a < x]
                  larger  = [b | b <- xs, b > x]
