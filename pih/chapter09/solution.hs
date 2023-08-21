module Chapter09Solutions where

-- 1.
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [ys | xss <- subs xs, ys <- perms xss]

-- 2.
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ []     = []
removeFirst x (y:ys) | x == y    = ys
                     | otherwise = y : removeFirst x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _          = True
isChoice (x:xs) ys = elem x ys && isChoice xs (removeFirst x ys)

-- 3.
-- The execution of the function continues without ending.
-- The reason is that when the expression is divided into two,
-- one side has an empty list and the other side has the original list as it is.

-- 4.
data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where brak (Val n) = show n
                             brak e       = "(" ++ show e ++ ")"

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int]-> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                 l <- exprs ls,
                 r <- exprs rs,
                 e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

possibleExpressions = length [e | xs <- choices [1,3,7,10,25,50], e <- exprs xs]
successfulExpressions = length [e | xs <- choices [1,3,7,10,25,50], e <- exprs xs, (not . null . eval) e]

-- 5.
-- The definition of the valid function should be modified as follows.
valid' :: Op -> Int -> Int -> Bool
valid' Add _ _ = True
valid' Sub _ _ = True
valid' Mul _ _ = True
valid' Div x y = y /= 0 && x `mod` y == 0

expressions = length [e | xs <- choices [1,3,7,10,25,50], e <- exprs xs, (not . null . eval) e]

-- 6.a
data Op' = Add' | Sub' | Mul' | Div' | Exp'

instance Show Op' where
    show Add' = "+"
    show Sub' = "-"
    show Mul' = "*"
    show Div' = "/"
    show Exp' = "^"

valid'' :: Op' -> Int -> Int -> Bool
valid'' Add' x y = x <= y
valid'' Sub' x y = x > y
valid'' Mul' x y = x /= 1 && y /= 1 && x <= y
valid'' Div' x y = y /= 1 && x `mod` y == 0
valid'' Exp' x y = y /= 1

apply' :: Op' -> Int -> Int -> Int
apply' Add' x y = x + y
apply' Sub' x y = x - y
apply' Mul' x y = x * y
apply' Div' x y = x `div` y
apply' Exp' x y = x ^ y

ops' = [Add', Sub', Mul', Div', Exp']
