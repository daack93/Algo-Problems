import PriorityQueue

main = do
    --x <- readLn :: IO Int
    print (takeBounds 1 1000 genPrimes)
    --solveX x solution

solution = do
    [a,b] <- fmap (map read.words) getLine
    print (takeBounds a b genPrimes)

solveX x solution = do
    if (x > 0) then do
        solution
        solveX (x-1) solution
        return ()
    else return ()

takeBounds :: Int -> Int -> [Int] -> [Int]
takeBounds a b [] = []
takeBounds a b (x:xs)
    |b < x        = []
    |a > x        = (takeBounds a b xs)
    |otherwise    = x : (takeBounds a b xs)

genPrimes = 2:3:5:7:(sieve (spin wheel2357 11))

wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8
            :6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357
spin (x:xs) n = n : spin xs (n + x)

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve' xs (addPrime x EmptyQueue)
    where
        addPrime :: Int -> PriorityQueue Int [Int] -> PriorityQueue Int [Int]
        addPrime x queue = insert (x*x) (map (*x) xs) queue
        sieve' :: [Int] -> PriorityQueue Int [Int] -> [Int]
        sieve' [] queue = []
        sieve' (x:xs) queue
            | nextComp <= x = sieve' xs (adjust queue)
            | otherwise     = x : sieve' xs (addPrime x queue)
            where
                nextComp = head (peep queue)
                adjust queue
                    | n <= x     = adjust (insert (head ns) (tail ns) (pop queue))
                    | otherwise  = queue
                    where
                        (n,ns) = ((head (peep queue)),(tail (peep queue)))


