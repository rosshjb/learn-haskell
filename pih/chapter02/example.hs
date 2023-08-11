module Chapter02Examples where

{-
double
and
quadruple
-}
double x = x + x

quadruple x = double (double x)

-- Factorial of a positive integer:
factorial n = product [1..n]

-- Average of a list of integers:
average ns = sum ns `div` length ns

a = b + c
    where
      b = 1
      c = 2
d = a * 2

a' = b' + c'
     where
      {b' = 1;
       c' = 2}
d' = a' * 2

a'' = b'' + c'' where {b'' = 1; c'' = 2}; d'' = a'' * 2


