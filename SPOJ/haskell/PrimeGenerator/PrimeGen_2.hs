--A fast Prime Generator Based on Melisa ONeal's paper:
--https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
--Author: David Ackerman
--April 19th 2016
--
--However, this does not solve the SPOJ PRIMES Problem, since it is
--performing the sieve from 2 no matter what. Need to rethink in order to
--get large ranges like 999900000-1000000000.
--Possibly primeGenRange, to perform sieve up to rad b and then continue from a?

import PriorityQueue

main = do
    x <- readLn :: IO Int
    print $ length (lessThan x genPrimes)
    --solveX x solution

lessThan m [] = []
lessThan m (x:xs)
    | x <= m    = x : (lessThan m xs)
    | otherwise = []

solution = do
    [a,b] <- fmap (map read.words) getLine
    printInBounds a b genPrimes

solveX x solution = do
    if (x > 0) then do
        solution
        solveX (x-1) solution
        return ()
    else return ()

printInBounds a b (x:xs)
    |b < x        = return ()
    |a > x        = do 
            printInBounds a b xs
            return ()
    |otherwise    = do
        print x
        printInBounds a b xs

genPrimes = 2:3:5:7:(sieve (spin wheel2357 11))

wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8
            :6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357
spin (x:xs) n = n : spin xs (n + x)

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve' xs (addPrime x EmptyQueue)
    where
        addPrime :: Int -> PriorityQueue Int [Int] (Int,Int) -> PriorityQueue Int [Int] (Int,Int)
        addPrime x queue = insert (x*x) (map (*x) xs) queue
        sieve' :: [Int] -> PriorityQueue Int [Int] (Int,Int) -> [Int]
        sieve' [] queue = []
        sieve' (x:xs) queue
            | nextComp <= x = sieve' xs (adjust queue)
            | otherwise     = x : sieve' xs (addPrime x queue)
            where
                nextComp = head (readMin queue)
                adjust queue
                    | n <= x     = adjust (insert (head ns) (tail ns) (removeMin queue))
                    | otherwise  = queue
                    where
                        (n,ns) = ((head (readMin queue)),(tail (readMin queue)))


