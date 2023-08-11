module Chapter01Examples where
import Prelude hiding (sum)

double x = x + x

sum :: Num a => [a] -> a
sum []     = 0
sum (n:ns) = n + sum ns

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, x < b]

seqn :: [IO a] -> IO [a]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

seqn' :: Monad m => [m a] -> m [a]
seqn' [] = return []
seqn' (act:acts) = do x <- act
                      xs <- seqn' acts
                      return (x:xs)
