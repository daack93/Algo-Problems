main = do
    x <- readLn :: IO Int
    print (take x (spin wheel2357 11))
    --print (map (*5) (enumFrom 2))
    --solveX x solution

solution = do
    [a,b] <- fmap (map read.words) getLine
    print (a + b)

solveX x solution = do
    if (x > 0) then do
        solution
        solveX (x-1) solution
        return ()
    else return ()



wheel2357 = 2:4:2:4:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:8
            :6:4:6:2:4:2:6:6:4:2:4:6:4:2:4:2:4:2:10:2:10:wheel2357

spin (x:xs) n = n : spin xs (n + x)

sieve [] = []
sieve (x:xs) = x : sieve' xs (Leaf x)
    where
        sieve' [] queue = []
        sieve' (x:xs) queue
            | nextComp <= x = sieve' xs (adjust queue)
            | otherwise     = x : sieve' xs (addPrime x queue)
            where
                nextComp = peep queue
                addPrime x queue = insert (map (*x) xs) queue
                adjust queue
                    | (peep queue) <= x     = adjust (pop queue)
                    | otherwise  = queue


data PQTree a = Node (PQTree a) (PQTree a) a | Branch (PQTree a) a | Leaf a


insertList [] a = a
insertList (x:xs) a = insertList xs (insert x a)

insert x (Leaf a)
    | x < a             = Branch (Leaf a) x
    | otherwise         = Branch (Leaf x) a
insert x (Branch r a)
    | x < a             = Node r (Leaf a) x
    | otherwise         = Node r (Leaf x) a
insert x (Node r l a)
    | depth r <= depth l = Node (insert (min x a) r) l (max x a)
    | otherwise          = Node r (insert (min x a) l) (max x a)

depth (Leaf a) = 1
depth (Branch r a) = 1 + depth r
depth (Node r l a) = 1 + depth r

peep (Leaf a) = a
peep (Branch _ a) = a
peep (Node _ _ a) = a

pop (Branch l a) = l
pop (Node r l a)
    | peep r < peep l  = if depth r > 1 then Node (pop r) l (peep r) else Branch l (peep r)
    | otherwise        = if depth l > 1 then Node r (pop l) (peep l) else Branch r (peep l)
