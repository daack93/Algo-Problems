main = do 
    line <- getLine
    if "42" /= line
        then do
            putStrLn line
            main
        else return ()
