-- Priority Queue Data Structure
-- Author: David Ackerman
-- Apr 18, 2016
-- Another attempt, simpler data structures, better rank

module PriorityQueue (PriorityQueue(EmptyQueue),readMin,insert,removeMin) where

data PriorityQueue k v d = 
          EmptyQueue
        | Node (PriorityQueue k v d) (PriorityQueue k v d) k v d
        --each node stores key, value, and (rank,depth) priority

rank :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> d
rank EmptyQueue = 0
rank (Node l r k v d) = fst d

depth :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> d
depth EmptyQueue = 0
depth (Node l r k v d) = snd d

readMin :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> v
readMin EmptyQueue = error "Reading EmptyQueue"
readMin (Node l r k v d) = v

readMinK :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> k
readMinK EmptyQueue = error "Reading EmptyQueue"
readMinK (Node l r k v d) = k

insert :: (Ord k, Integral d) => k -> v -> PriorityQueue k v (d,d) -> PriorityQueue k v (d,d)
insert k' v' EmptyQueue = Node EmptyQueue EmptyQueue k' v' (1,1)
insert k' v' (Node l r k v d)
    |rr < rl        = Node oldNode newNode newK newV (newRank,newDepth)
    |otherwise      = Node newNode oldNode newK newV (newRank,newDepth)
                where
                    rl = rank l
                    rr = rank r
                    oldNode = if rr < rl then l else r
                    replacedNode = if rr < rl then r else l
                    newNode = if k' < k then insert k v replacedNode else insert k' v' replacedNode
                    newRank = 1 + (min (rank newNode) (rank oldNode))
                    newDepth = 1 + (max (depth newNode) (depth oldNode))
                    (newK, newV) = if k' < k then (k',v') else (k,v)

empty EmptyQueue = True
empty n = False

--replace with min, and bubble down. Not sure how to do this :( --
removeMin :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> PriorityQueue k v (d,d)
removeMin EmptyQueue = error "Removing from EmptyQueue"
removeMin n@(Node l r k v d) = popLast (bubble (Node l r k' v' d))
    where
        bubble n@(Node l r k v d)
            | empty l               = n
            | empty r, k <= lk      = n
            | empty r               = Node (Node EmptyQueue EmptyQueue k v (0,0)) EmptyQueue (readMinK l) (readMin l) (0,1)
            | k <= lk, k <= rk      = n
            | lk <= rk              = Node (bubble (Node ll lr k v ld)) r lk lv d
            | otherwise             = Node l (bubble (Node rl rr k v rd)) rk rv d
            where
                getVals (Node l r k v d) = (l,r,k,v,d)
                (ll,lr,lk,lv,ld) = getVals l
                (rl,rr,rk,rv,rd) = getVals r
        popLast n@(Node l r k v d)
            | empty l           = EmptyQueue
            | empty r           = lessDeep n
            | depth l > depth r = Node pl r k v (min (rank pl) (rank r), max (depth pl) (depth r))
            | otherwise         = Node l pr k v (min (rank l) (rank pr), max (depth l) (depth pr))
            where 
                lessDeep (Node l r k v d) = Node l r k v (fst d, (snd d) - 1)
                pr = popLast r
                pl = popLast l
        (k',v') = getLast n
            where
                getLast n@(Node l r k v d)
                    | empty l            = (k,v)
                    | empty r            = getLast l
                    | depth l > depth r  = getLast l
                    | otherwise          = getLast r



