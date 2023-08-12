module Chapter04Examples where
import Prelude hiding (even, splitAt, recip, abs,
    signum, not, (&&), fst, snd, head, tail, const,
    sum)

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1 / n

abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n < 0 then -1 else
               if n == 0 then 0 else 1

abs' :: Int -> Int
abs' n | n >= 0    = n
       | otherwise = -n

signum' :: Int -> Int
signum' n | n < 0  = -1
          | n == 0 = 0
          | n > 0  = 1

not :: Bool -> Bool
not False = True
not True  = False

(&&) :: Bool -> Bool -> Bool
True && True   = True
_    && _      = False

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

test :: [Char] -> Bool
test ['a',_,_] = True
test _         = False

test' :: [Char] -> Bool
test' ('a':_) = True
test' _       = False

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

const :: a -> b -> a
const x _ = x

const' :: a -> (b -> a)
const' x = \_ -> x

odds :: Int -> [Int]
odds n = map f [0..n-1]
         where f x = x * 2 + 1

odds' :: Int -> [Int]
odds' n = map (\x -> x * 2 + 1) [0..n-1]

addition = (+)
addition' = \x -> (\y -> x + y)

successor = (1+)
successor' = \y -> 1 + y

reciprocation = (1/)
reciprocation' = \y -> 1/y

doubling = (*2)
doubling' = \x -> x * 2

halving = (/2)
halving' = \x -> x / 2

sum :: [Int] -> Int
sum = foldl (+) 0
