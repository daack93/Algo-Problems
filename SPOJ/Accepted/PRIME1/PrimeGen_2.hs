main = do
    x <- readLn :: IO Int
    print (map (*5) (enumFrom 2))
    --solveX x solution

solution = do
    [a,b] <- fmap (map read.words) getLine
    print (genPrimes a b)

solveX x solution = do
    if (x > 0) then do
        solution
        solveX (x-1) solution
        return ()
    else return ()


genPrimes a b
    |a < 3     = [2] + filtered [3, 5..n]
    |otherwise = filtered [m, (m+2)..n]
       where
           m = if odd a then a else (a + 1)
           n = if odd b then b else (b - 1)
           filtered xs = [ x | x <- xs, prime x 3]
           prime x i = x < i*i || (x `mod` i /= 0 && prime x (i+1))

    
