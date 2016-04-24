-- Another attempt at PQs, this time a Skew Binomial Q rather than Binary Heap
-- Author: David Ackerman
-- Date: Apr 21 2016
-- Trying to follow http://www.brics.dk/RS/96/37/BRICS-RS-96-37.pdf, but
-- I'm not sure how he assures that the subtrees are sorted.

main = do
    let q = insertL ( reverse [1..25]) E
    print "start"
    print (rq q)
    print "loaded"
    pC q
    print "================"
    pQ q 0 0 
    print "================"
    pQ (dq q) 0 0 
    print "================"
    pQ (dq (dq q)) 0 0 
    print "================"
    pQ (dq (dq (dq q))) 0 0 
    print "================"
    pQ (dq (dq (dq (dq (dq (dq (dq q))))))) 0 0 
    print "================"
    print "done"

insertL [] q = q
insertL (x:xs) q = iq x x (insertL xs q)

pC q@(T c _ _ _) = pC' c
    where
        pC' (x@(T _ _ v _) : xs) = do
            print v
            pC' xs
        pC' [] = return ()



pQ q@(T xs _ v r) d t
    | d > 0 = do 
        putStr "  " 
        pQ q (d-1) t
    | otherwise = do
        print v
        pL xs
            where
                pL [] = return ()
                pL (x:xs) = do
                    pQ x (t+1) (t+1)
                    pL xs
pQ _ d t = return ()
        


-- module BinomialQueue (SBTQ(E),iq,rq,dq,rk,sq) where

data SBTQ k v = E | T [SBTQ k v] k v Int
nq k v = T [] k v 0
sl :: Ord k => SBTQ k v -> SBTQ k v -> SBTQ k v
sl n@(T c k v r) o@(T d l w s) = T (n' : c') k' v' (1 + r')
    where
        (n',c',k',v',r') = if k < l then (o,c,k,v,r) else (n,d,l,w,s)
sk :: Ord k => SBTQ k v -> SBTQ k v -> SBTQ k v -> SBTQ k v
sk n@(T c k v r) o@(T d l w s) p@(T e m x t) = T (n' : o' : c') k' v' (1 + r')
    where
        (n',o',c',k',v',r')
            | k <= (min l m) = (o,p,c,k,v,s)
            | l < m          = (n,p,d,l,w,s)
            | otherwise      = (n,o,e,m,x,t)
iq :: Ord k => k -> v -> SBTQ k v -> SBTQ k v
iq k v (T (x@(T _ _ _ r):y@(T _ _ _ s):xs) l w _) = T c' k' v' 0
    where
        (k',v',l',w') = if k < l then (k,v,l,w) else (l,w,k,v)
        c'
            | r == s    = (sk (nq l' w') x y) : xs
            | otherwise = (nq l' w') : x : y : xs
iq k v (T c l w _) = T ((nq l' w') : c) k' v' 0
    where
        (l',w',k',v') = if k < l then (l,w,k,v) else (k,v,l,w)
iq k v E = nq k v
m :: Ord k => SBTQ k v -> SBTQ k v -> SBTQ k v
m n@(T c k v _) o@(T d l w _) = iq k' v' (T (m' c d) l' w' 0)
    where
        (k',v',l',w') = if k < l then (k,v,l,w) else (l,w,k,v)
        m' :: Ord k => [SBTQ k v] -> [SBTQ k v] -> [SBTQ k v]
        m' c@(x@(T _ _ _ r):xs) d@(y@(T _ _ _ s):ys) = x' : (m' d' e')
            where
            (x',d',e')
                | r < s = (x,xs,d)
                | s < r = (y,ys,c)
                | otherwise = (sl x y,xs,ys)
        m' c d = c ++ d
m n o = n
rq :: Ord k => SBTQ k v -> v
rq n@(T _ _ v _) = v
rk n@(T _ k _ _) = k
sq k v n@(T c _ _ _) = T c k v 0
sq k v n = n
dq :: Ord k => SBTQ k v -> SBTQ k v 
dq n@(T c _ _ _) = d' c
    where
        d' :: Ord k => [SBTQ k v] -> SBTQ k v
        d' c@(x@(T _ k v r):xs)
            | r > 0 = m x (d' xs)
            | otherwise = iq k v (d' xs)
        d' c = E
