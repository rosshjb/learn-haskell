module Chapter06Examples where

fac :: Int -> Int
fac n = product [1..n]

fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac' (n - 1)

multiplication :: Int -> Int -> Int
m `multiplication` 0 = 0
m `multiplication` n = m + (m * (n-1))

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

(+++) :: [a] -> [a] -> [a]
[]     +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n-1) xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where smaller = [x' | x' <- xs, x' <= x]
                     larger  = [x' | x' <- xs, x' > x]

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)

evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds []     = []
odds (x:xs) = evens xs

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

drop'' :: Int -> [a] -> [a]
drop'' 0 xs     = xs
drop'' _ []     = []
drop'' n (_:xs) = drop (n-1) xs

init' :: [a] -> [a]
init' [_]    = []
init' (x:xs) = x : init' xs
