module Chapter08Solutions where

-- 1.
data Nat = Zero | Succ Nat
         deriving Show

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ n) m = Succ (add n m)

mul :: Nat -> Nat -> Nat
mul m Zero     = Zero
mul m (Succ n) = add m (mul m n)

-- 2.
-- This implementation requires fewer comparison operations.
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4))
         5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                        EQ -> True
                        LT -> occurs x l
                        GT -> occurs x r 

-- 3.
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
             deriving Show

t' :: Tree' Int
t' = Node' (Node' (Leaf' 1) (Leaf' 4))
          (Node' (Leaf' 6) (Leaf' 9))

leaves :: Tree' a -> Int
leaves (Leaf' _)   = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _)   = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- 4.
halves :: [a] -> ([a],[a])
halves xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs  = Node' (balance left) (balance right)
              where left  = (fst . halves) xs
                    right = (snd . halves) xs

-- 5.
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 6.
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

-- 7.
data Maybe' a = Nothing' | Just' a

instance Eq a => Eq (Maybe' a) where
    Just' a == Just' b   = a == b
    Nothing' == Nothing' = True
    _ == _               = False

data List a = Nil | Cons a (List a)

instance Eq a => Eq (List a) where
    Cons x xs == Cons y ys = (x == y) && (xs == ys)
    Nil == Nil             = True
    _ == _                 = False

-- 8.
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | BiImply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- A or (B and C)
p5 :: Prop
p5 = Or (Var 'A') (And (Var 'B') (Var 'C'))

-- A or (not A)
p6 :: Prop
p6 = Or (Var 'A') (Not (Var 'A'))

-- (A <-> A)
p7 :: Prop
p7 = BiImply (Var 'A') (Var 'A')

-- (A <-> B)
p8 :: Prop
p8 = BiImply (Var 'A') (Var 'B')

type Assoc k v = [(k,v)]

type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k' == k]

eval' :: Subst -> Prop -> Bool
eval' _ (Const b)     = b
eval' s (Var x)       = find x s
eval' s (Not p)       = not (eval' s p)
eval' s (And p q)     = eval' s p && eval' s q
eval' s (Imply p q)   = eval' s p <= eval' s q
eval' s (Or p q)      = eval' s p || eval' s q
eval' s (BiImply p q) = eval' s p == eval' s q

vars :: Prop -> [Char]
vars (Const _)     = []
vars (Var x)       = [x]
vars (Not p)       = vars p
vars (And p q)     = vars p ++ vars q
vars (Imply p q)   = vars p ++ vars q
vars (Or p q)      = vars p ++ vars q
vars (BiImply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
          where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs            = rmdups (vars p)
                 rmdups []     = []
                 rmdups (x:xs) = x : rmdups (filter (/= x) xs)

isTaut :: Prop -> Bool
isTaut p = and [eval' s p | s <- substs p]

-- 9.
data Expr' = Val' Int | Add' Expr' Expr' | Mul' Expr' Expr'

type Cont = [Op]

data Op = EVALA Expr' | EVALM Expr' | ADD Int | MUL Int

eval'' :: Expr' -> Cont -> Int
eval'' (Val' n)   c = exec   c n
eval'' (Add' x y) c = eval'' x (EVALA y : c)
eval'' (Mul' x y) c = eval'' x (EVALM y : c)

exec :: Cont -> Int -> Int
exec []            n = n
exec (EVALA y : c) n = eval'' y (ADD n : c)
exec (EVALM y : c) n = eval'' y (MUL n : c)
exec (ADD n : c)   m = exec   c (n+m)
exec (MUL n : c)   m = exec   c (n*m)

value :: Expr' -> Int
value e = eval'' e []

