main = do
    x <- readLn :: IO Int
    solveX x

solveX x =
    if x > 0 then do
        n <- getLine
        putStrLn (walkThrough n (reverse n) (length n) 1)
        solveX (x - 1)
        return ()
    else return ()

nextPal x = if palin x then x else nextPal (x + 1)
palin x = (show x) == (reverse (show x))

walkThrough :: [Char] -> [Char] -> Int -> Int -> [Char]
walkThrough [] ys l c = []
walkThrough xs [] l c = []
walkThrough (x:xs) (y:ys) l c=
    if even l then
        if length xs > l `div` 2 then
            [x] ++ (walkThrough xs ys l (getC x (y+c)))
        else if length xs > (l `div` 2) - 2 then
            [max x (y+c)] ++ (walkThrough xs ys l c)
        else [y] ++ (walkThrough xs ys l c)
    else 
        if length xs >= l `div` 2 then
            [x] ++ (walkThrough xs ys l (getC x (y+c)))
        else if length xs > (l `div` 2) - 1 then
            [max x (y+c)] ++ (walkThrough xs ys l c)
        else [y] ++ (walkThrough xs ys l c)

    where getC a b = if a < b then 1 else 0

