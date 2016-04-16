main = do
    x <- readLn :: IO Int
    solveX x solution

solution = do
    [a,b] <- fmap (map read.words) getLine
    mapM printIfPrime [a .. b]

solveX x solution = do
    if (x > 0) then do
        solution
        solveX (x-1) solution
        return ()
    else return ()

printIfPrime x = do
    if (isPrime x) then
        print x
    else return ()


isPrime x = isPrimeHelper 2 x

isPrimeHelper x y = do
    if (y < 2) then False else do
    if (x*x <= y) then do
        if (mod y x == 0) then False
        else isPrimeHelper (x + 1) y
    else True
