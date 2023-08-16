module Chapter07Examples where
import Data.Char
import Data.List

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' f []     = []
map'' f (x:xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p []                 = []
filter'' p (x:xs) | p x       = x : filter'' p xs 
                  | otherwise = filter'' p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

or' :: [Bool] -> Bool
or' []     = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

or'' :: [Bool] -> Bool
or'' = foldr (||) False

and'' :: [Bool] -> Bool
and'' = foldr (&&) True

sum''' :: Num a => [a] -> a
sum''' xs = foldr (+) 0 xs

product''' :: Num a => [a] -> a
product''' xs = foldr (*) 1 xs

or''' :: [Bool] -> Bool
or''' xs = foldr (||) False xs

and''' :: [Bool] -> Bool
and''' xs = foldr (&&) True xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr (\_ n -> 1+n) 0

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' []     = []
reverse'' (x:xs) = snoc x (reverse'' xs)

reverse''' :: [a] -> [a]
reverse''' = foldr snoc []

sum'''' :: Num a => [a] -> a
sum'''' = sum''''' 0
          where
            sum''''' v []     = v
            sum''''' v (x:xs) = sum''''' (v+x) xs

sum''''' :: Num a => [a] -> a
sum''''' = foldl (+) 0

product'''' :: Num a => [a] -> a
product'''' = foldl (*) 1

or'''' :: [Bool] -> Bool
or'''' = foldl (||) False

and'''' :: [Bool] -> Bool
and'''' = foldl (&&) True

length''' :: [a] -> Int
length''' = foldl (\n _ -> n + 1) 0

reverse'''' :: [a] -> [a]
reverse'''' = foldl (\xs x -> x:xs) []

foldl_ :: (a -> b -> a) -> a -> [b] -> a
foldl_ f v []     = v
foldl_ f v (x:xs) = foldl_ f (f v x) xs

composition :: (b -> c) -> (a -> b) -> (a -> c)
f `composition` g = \x -> f (g x)

odd' :: Integral a => a -> Bool
odd' n = not (even n)

twice' :: (a -> a) -> a -> a
twice' f x = f (f x)

sumsqreven' :: [Int] -> Int
sumsqreven' ns = sum (map (^2) (filter even ns))

odd'' :: Integral a => a -> Bool
odd'' = not . even

twice'' :: (a -> a) -> a -> a
twice'' f = f . f

sumsqreven'' :: [Int] -> Int
sumsqreven'' = sum . map (^2) . filter even

id' :: a -> a
id' = \x -> x

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int') . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
             [c]    -> c
             (c:cs) -> winner' (elim c bs)

