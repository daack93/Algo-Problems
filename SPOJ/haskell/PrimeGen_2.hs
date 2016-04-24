--SPOJ Problem: PRIME1
--Author: David Ackerman
--April 19th 2016

import BinomialQueue
import Debug.Trace

main = do
    x <- readLn :: IO Int
    solveX x solution

solution = do
    [a,b] <- fmap (map read.words) getLine
    -- print $ length $ genPrimes a b
    printEm  (genPrimes a b)
    -- print $ length $ toKeyList (table a b (spin wheel2357 11))

printq E = return ()
printq n = do
    print $ (head (rq n)) `div` 13
    printq $ dq n


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

toList E = []
toList q = (head (rq q)) : (toList (dq q))



genPrimes a b = [x | x <- [2,3,4,7], x >= a, x <= b] ++ (sieve a b (spin wheel2357 ( max 11 (11+(210*((a-11) `div` 210))))))

wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8
            :6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357
spin (x:xs) n = n : spin xs (n + x)

sieve :: Int -> Int -> [Int] -> [Int]
sieve a b [] = []
sieve a b xs | trace ("Calling sieve") False = undefined
sieve a b xs = (sieve' a b xs (table a b (spin wheel2357 11)))
    where
        sieve' :: Int -> Int -> [Int] -> SBTQ Int [Int] -> [Int]
        sieve' a b [] queue = []
        sieve' a b (x:xs) queue
            | nextComp <= x = sieve' a b xs (adjust queue)
            | x > b         = []
            | x < a         = sieve' a b xs queue
            | otherwise     = x : sieve' a b xs queue
            where
                nextComp = rk queue
                adjust queue
                    | n <= x     = adjust (iq (head ns) (tail ns) (dq queue))
                    | otherwise  = queue
                    where
                        (n,ns) = (rk queue,rq queue)

table :: Int -> Int -> [Int] -> SBTQ Int [Int]
table a b (x:xs) = addPrime x (table' a b xs (addPrime x E))
    where
        addPrime :: Int -> SBTQ Int [Int] -> SBTQ Int [Int]
        -- addPrime x queue | trace ("addPrime " ++ show x) False = undefined
        addPrime x queue = iq (x*x) (map (*x) xs) queue
        addTPrime x a queue = iq (x*s) (map (*x) (greater s xs)) queue
            where
                s = a `div` x
                greater :: Int -> [Int] -> [Int]
                greater s (x:xs)
                    | x > s     = x : xs
                    | otherwise = (greater s xs)

        table' :: Int -> Int -> [Int] -> SBTQ Int [Int] -> SBTQ Int [Int]
        table' a b [] queue = E
        -- table' a b (x:xs) queue | trace ("table'" ++ show x ++ " " ++ show nextComp) False = undefined
            where
                nextComp = (rk queue)
        table' a b (x:xs) queue
            | nextComp <= x = table' a b xs (adjust queue)
            | x*x > b       = E
            | otherwise     = addTPrime x a (table' a b xs  (addPrime x queue))
            where
                nextComp = rk queue
                -- adjust q | trace ("adjusting out " ++ show n) False = undefined
                  --  where
                  --      n = rk q
                adjust queue
                    | n <= x     = adjust (iq (head ns) (tail ns) (dq queue))
                    | otherwise  = queue
                    where
                        (n,ns) = (rk queue,rq queue)
