main = do
    x <- readLn :: IO Int
    print (map (*5) (enumFrom 2))
    --solveX x solution

solution = do
    [a,b] <- fmap (map read.words) getLine
    print ([a .. b] :: [Int])

solveX x solution = do
    if (x > 0) then do
        solution
        solveX (x-1) solution
        return ()
    else return ()



    
