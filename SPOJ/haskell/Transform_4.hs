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

polish (x:xs) =
    if (length xs > 0) then
        if operator x then
            (polish xs) ++ [x]
        else if x == '(' then (polish before) ++ (polish after)
        else if x == ')' then polish xs
        else [x] ++ polish xs
    else if x /= ')' then [x]
    else []
        
    where
        operator x = x == '*' || x == '+' || x == '-' || x == '/' || x == '^'
        before = toEnd xs
        after = fromEnd xs

toEnd (x:xs) = 
    if length xs > 0 then
        if x == ')' then
            []
        else if x == '(' then
            ['('] ++ (toEnd xs) ++ [')'] ++ (toEnd (fromEnd xs))
        else
            [x] ++ (toEnd xs)
    else
        []

fromEnd (x:xs) = 
    if length xs > 0 then
        if x == ')' then
            ")" ++ xs
        else if length xs > 1 && x == '(' then
            ")" ++ fromEnd (drop 1 (fromEnd xs))
        else ")" ++ fromEnd xs
    else [')']

