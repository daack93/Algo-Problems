import Data.Char

main = do
    x <- readLn :: IO Int
    solveX x

solveX x =
    if x > 0 then do
        n <- getLine
        putStrLn (walkThrough n)
        solveX (x - 1)
        return ()
    else return ()

nextPal x = if palin x then x else nextPal (x + 1)
palin x = (show x) == (reverse (show x))

walkThrough :: [Char] ->  [Char]
walkThrough xs = (walkThrough' xs (reverse xs) (length xs) 1)
    where
        getC a b  = if a < b then 1 else 0
        walkThrough' :: [Char] -> [Char] -> Int -> Int -> [Char]
        walkThrough' [] ys l c = []
        walkThrough' xs [] l c = []
        walkThrough' (x:xs) (y:ys) l c
            | even l    =
                if length xs > l `div` 2 then
                    [x] ++ (walkThrough' xs ys l (getC (digitToInt x) ((digitToInt y)+c)))
                else if length xs > (l `div` 2) - 1 then
                    [max y (intToDigit ((digitToInt x)+c))] ++ (walkThrough' xs ys l c)
                else if length xs > (l `div` 2) - 2 then
                    [max x (intToDigit ((digitToInt y)+c))] ++ (walkThrough' xs ys l c)
                else [y] ++ (walkThrough' xs ys l c)
            | odd l     =
                if length xs >= l `div` 2 then
                    [x] ++ (walkThrough' xs ys l (getC (digitToInt x) ((digitToInt y)+c)))
                else if length xs > (l `div` 2) - 1 then
                    [max x (intToDigit ((digitToInt y)+c))] ++ (walkThrough' xs ys l c)
                else [y] ++ (walkThrough' xs ys l c)


