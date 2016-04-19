--SPOJ Problem: PRIME1
--Author: David Ackerman
--April 19th 2016

import PriorityQueue

main = do
    x <- readLn :: IO Int
    solveX x solution

solution = do
    [a,b] <- fmap (map read.words) getLine
    printq  $  (table a b (spin wheel2357 11))
    print $ toKeyList (table a b (spin wheel2357 11))

printq EmptyQueue = return ()
printq n = do
    print $ readMinK n
    printq $ removeMin n


solveX x solution = do
    if (x > 0) then do
        solution
        solveX (x-1) solution
        return ()
    else return ()

printEm [] = return ()
printEm (x:xs) = do
    print x
    printEm xs



genPrimes a b = [x | x <- [2,3,4,7], x >= a, x <= b] ++ (sieve a b (spin wheel2357 ( max 11 (11+(210*((a-11) `div` 210))))))

wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8
            :6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357
spin (x:xs) n = n : spin xs (n + x)

sieve :: Int -> Int -> [Int] -> [Int]
sieve a b [] = []
sieve a b xs = (sieve' a b xs (table a b (spin wheel2357 11)))
    where
        sieve' :: Int -> Int -> [Int] -> PriorityQueue Int [Int] (Int,Int) -> [Int]
        sieve' a b [] queue = []
        sieve' a b (x:xs) queue
            | nextComp <= x = sieve' a b xs (adjust queue)
            | x > b         = []
            | x < a         = sieve' a b xs queue
            | otherwise     = x : sieve' a b xs queue
            where
                nextComp = head (readMin queue)
                adjust queue
                    | n <= x     = adjust (insert (head ns) (tail ns) (removeMin queue))
                    | otherwise  = queue
                    where
                        (n,ns) = ((head (readMin queue)),(tail (readMin queue)))

table :: Int -> Int -> [Int] -> PriorityQueue Int [Int] (Int,Int)
table a b (x:xs) = addTablePrime a x (table' a b xs (addPrime x EmptyQueue))
    where
        addPrime :: Int -> PriorityQueue Int [Int] (Int,Int) -> PriorityQueue Int [Int] (Int,Int)
        addPrime x queue = insert (x*x) (map (*x) xs) queue

        addTablePrime :: Int -> Int -> PriorityQueue Int [Int] (Int,Int) -> PriorityQueue Int [Int] (Int,Int)
        addTablePrime a x queue = insert (x) (greater a (map (*x) xs)) queue
            where
                s = max (a `div` x) x
                greater :: Int -> [Int] -> [Int]
                greater s (x:xs)
                    | x >= s     = x : xs
                    | otherwise = (greater s xs)

        table' :: Int -> Int -> [Int] -> PriorityQueue Int [Int] (Int,Int) -> PriorityQueue Int [Int] (Int,Int)
        table' a b [] queue = EmptyQueue
        table' a b (x:xs) queue
            | nextComp <= x = table' a b xs (adjust queue)
            | x*x > b       = EmptyQueue
            | otherwise     = addTablePrime a x (table' a b xs (addPrime x queue))
            where
                nextComp = head (readMin queue)
                adjust queue
                    | n <= x     = adjust (insert (head ns) (tail ns) (removeMin queue))
                    | otherwise  = queue
                    where
                        (n,ns) = ((head (readMin queue)),(tail (readMin queue)))
