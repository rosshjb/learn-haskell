module Chapter07Solutions where
import Data.Char

-- 1.
list_comprehension :: (a -> Bool) -> (a -> b) -> [a] -> [b]
list_comprehension p f = map f . filter p

-- 2.a
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

-- 2.b
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

-- 2.c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                 = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []

-- 2.d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                 = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x : xs

-- 3.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x v -> f x : v) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x v -> case p x of
                           True      -> x : v
                           otherwise -> v) []

-- 4.
dec2int :: [Int] -> Int
dec2int = foldl (\n x -> 10*n + x) 0

-- 5.
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' u = \x y -> u (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' c = \p -> c (fst p) (snd p)

-- 6.
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f

-- 7.
type Bit' = Int

bin2int :: [Bit'] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin' :: Int -> [Bit']
int2bin' 0 = []
int2bin' n = n `mod` 2 : int2bin' (n `div` 2)

make8 :: [Bit'] -> [Bit']
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit'] -> [Bit']
parity [] = []
parity bits = if (even . sum) bits then [0] ++ bits
                                   else [1] ++ bits

encode :: String -> [Bit']
encode = concat . map (parity . make8 . int2bin' . ord)

chop8' :: [Bit'] -> [[Bit']]
chop8' []   = []
chop8' bits = take 8 bits : chop8' (drop 8 bits)

unparity :: [Bit'] -> [Bit']
unparity []   = []
unparity bits = (if body == parity (tail body) then (tail body)
                else error "parity check error") ++ unparity (drop 9 bits)
                where body = take 9 bits

decode :: [Bit'] -> String
decode = map (chr . bin2int) . chop8' . unparity

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit'] -> [Bit']
channel = id

-- 8.
channel' :: [Bit'] -> [Bit']
channel' = tail

transmit' :: String -> String
transmit' = decode . channel' . encode

-- 9.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []     = []
altMap f g (x:xs) = f x : altMap g f xs

-- 10.
luhnDouble :: Int -> Int
luhnDouble x | doubled > 9 = doubled - 9
             | otherwise   = doubled
             where doubled = x * 2

luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . (altMap id luhnDouble) . reverse

