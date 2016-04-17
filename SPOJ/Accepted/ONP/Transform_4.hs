--SPOJ Problem 4: Transform an expression to reverse polish.
--I barely got this one done in HS, was about to give up!
--but you can avoid stacks with some tricky recursion.
--Author: David Ackerman
--Apr 16, 2016

main = do
    x <- readLn :: IO Int
    solveX x

solveX x =
    if x > 0 then do
        input <- getLine
        putStrLn $ polish input
        solveX (x - 1)
        return ()
    else return ()

polish :: [Char] -> [Char]
polish [] = []
polish ('(':xs) = (polish (toEnd xs)) ++ (polish (fromEnd xs))
polish (x:xs) = if operator x then (polish xs) ++ [x] else [x] ++ (polish xs)
        
operator :: Char -> Bool
operator x = x == '*' || x == '+' || x == '-' || x == '/' || x == '^'

toEnd :: [Char] -> [Char]
toEnd [] = []
toEnd ('(':xs) = "(" ++ (toEnd xs) ++ ")" ++ toEnd (fromEnd xs)
toEnd (')':xs) = []
toEnd (x:xs) = x : toEnd xs

fromEnd [] = []
fromEnd ('(':xs) = fromEnd (fromEnd xs)
fromEnd (')':xs) = xs
fromEnd (x:xs) = fromEnd xs

