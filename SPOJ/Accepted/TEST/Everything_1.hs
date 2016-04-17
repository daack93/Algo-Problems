--Spoj Problem 1: print the input untill it is 42
--Author: David Ackerman
--Apr 16, 2016

main = do 
    line <- getLine
    if "42" /= line
        then do
            putStrLn line
            main
        else return ()
