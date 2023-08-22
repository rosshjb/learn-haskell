module Chapter10Solutions where

import System.IO

-- 1.
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- 2.
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
                     where update r n = if r == row then n-num else n


putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "*"))

putBoard :: Board -> IO ()
putBoard board = putB 1
                 where putB r | r > (length board) = return ()
                              | otherwise          = do putRow r (board !! (r-1))
                                                        putB (r+1)

-- 3.
putBoard' :: Board -> IO ()
putBoard' board = sequence_ [putRow r n | (r,n) <- zip [1..] board]

-- 4.
adder :: IO ()
adder = do putStr "How many numbers? "
           str <- getLine
           total <- add 0 (read str)
           putStr "The total is "
           putStrLn (show total)
            
add :: Int -> Int -> IO Int
add total 0 = return total
add total n = do str <- getLine
                 add (total + (read str)) (n-1)

-- 5.
adder' :: IO ()
adder' = do putStr "How many numbers? "
            str <- getLine
            str2 <- sequence [getLine | _ <- [1..(read str :: Int)]]
            putStr "The total is "
            putStrLn (show (sum (map read str2)))

-- 6.
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = readLine' ""
           where readLine' :: String -> IO String
                 readLine' acc = do x <- getCh
                                    if x == '\n' then
                                       return acc
                                    else if x == '\DEL' then
                                       do putStr "\b\DEL\b"
                                          xs <- readLine' (if acc == [] then acc
                                                           else init acc)
                                          return xs
                                    else
                                       do putChar x
                                          xs <- readLine' (acc ++ [x])
                                          return xs
                 
