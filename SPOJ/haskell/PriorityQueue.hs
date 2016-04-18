-- Priority Queue Data Structure
-- Author: David Ackerman
-- Apr 18, 2016
-- Another attempt, simpler data structures, better rank

module PriorityQueue (PriorityQueue(EmptyQueue),readMin,insert,removeMin) where

data PriorityQueue k v d = 
          EmptyQueue
        | Node (PriorityQueue k v d) (PriorityQueue k v d) k v d

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


removeMin :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> PriorityQueue k v (d,d)
removeMin EmptyQueue = error "Removing from EmptyQueue"
removeMin (Node l r k v d)
    |takeRight,goRight   = Node l newNode (readMinK r) (readMin r) (min (fst d) (rank newNode), max (snd d) (depth newNode))
    |
    |otherwise                = Node l r k v d
    where
        goRight = (rank r < rank l) && (depth r <= depth l)
        takeRight = readMin r <= readMin l

