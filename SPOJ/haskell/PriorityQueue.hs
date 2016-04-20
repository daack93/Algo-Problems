-- Priority Queue Data Structure
-- Author: David Ackerman
-- Apr 20, 2016
-- A third attempt

-- module PriorityQueue (PriorityQueue(EmptyQueue),readMinK,readMin,insert,toKeyList,removeMin) where
--

data PriorityQueue k v d =
          EmptyQueue
        | Node (PriorityQueue k v d) (PriorityQueue k v d) (k, v, d, d)

empty :: Ord k => PriorityQueue k v Int -> Bool
empty EmptyQueue = True
empty q = False

right :: Ord k => PriorityQueue k v Int -> PriorityQueue k v Int
right EmptyQueue = error "Right on EmptyQueue"
right (Node r _ _) = r

left :: Ord k => PriorityQueue k v Int -> PriorityQueue k v Int
left EmptyQueue = error "Left on EmptyQueue"
left (Node _ l _) = l

key :: Ord k => PriorityQueue k v Int -> k
key EmptyQueue = error "key on EmptyQueue"
key (Node _ _ (k,_,_,_)) = k

val :: Ord k => PriorityQueue k v Int -> v
val EmptyQueue = error "val on EmptyQueue"
val (Node _ _ (_,v,_,_)) = v

rank :: Ord k => PriorityQueue k v Int -> Int
rank EmptyQueue = 0
rank (Node _ _ (_,_,r,_)) = r

depth :: Ord k => PriorityQueue k v Int -> Int
depth EmptyQueue = 0
depth (Node _ _ (_,_,_,d)) = d

constructNode :: Ord k => PriorityQueue k v Int -> PriorityQueue k v Int -> k -> v -> PriorityQueue k v Int
constructNode ln rn k v = Node ln rn (k,v,r,d)
    where
        r = 1 + (min (rank rn) (rank ln))
        d = 1 + (max (depth rn) (depth ln))

insert :: Ord k => k -> v -> PriorityQueue k v Int -> PriorityQueue k v Int
insert k v EmptyQueue = constructNode EmptyQueue EmptyQueue k v
insert k v (Node ln rn (j,u,_,_))
    | goRight       = constructNode ln (insert k' v' rn) j' u'
    | otherwise     = constructNode (insert k' v' ln) rn j' u'
    where
        goRight = (rank rn) < (rank ln)
        (k',v',j',u') = if k > j then (k,v,j,u) else (j,u,k,v)

removeMin :: Ord k => PriorityQueue k v Int -> PriorityQueue k v Int
removeMin EmptyQueue = error "removing from empty queue"
removeMin n@(Node rn ln _) = bubble (popLast (constructNode rn ln k' v'))
    where
        (k',v') = getLast n
            where
                getLast n@(Node rn ln (k,v,_,_))
                    | empty ln                = (k,v)
                    | (depth ln) > (depth rn) = getLast ln
                    | otherwise               = getLast rn
        popLast (Node ln rn (k,v,_,_))
            | empty ln                = EmptyQueue
            | (depth ln) > (depth rn) = constructNode (popLast ln) rn k v
            | otherwise               = constructNode ln (popLast rn) k v
        bubble EmptyQueue = EmptyQueue
        bubble n@(Node EmptyQueue _ _) = n
        bubble n@(Node (Node ll lr (lk,lv,_,_)) EmptyQueue (k,v,_,_))
            | k > lk        = constructNode (constructNode ll lr k v) EmptyQueue lk lv
            | otherwise     = n
        bubble n@(Node ln@(Node ll lr (lk,lv,_,_)) rn@( Node rl rr (rk,rv,_,_)) (k,v,_,_))
            | k <= lk, k <= rk = n
            | lk < rk          = constructNode (bubble (constructNode ll lr k v)) rn lk lv
            | otherwise        = constructNode ln (bubble (constructNode rl rr k v)) rk rv

main = do
    print "start"
    let q = insertValues ( reverse [1..1000]) EmptyQueue
    print "loaded"
    print $ toList q
    printq q
    print "end"

printq EmptyQueue = return ()
printq n = do
    print $ key n
    printq $ removeMin n

insertValues [] q = q
insertValues (x:xs) q = insertValues xs $ insert x x q

toList EmptyQueue = []
toList (Node r l (k,v,_,_)) = (k,v) : (toList l) ++ (toList r)
-- 
-- rank :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> d
-- rank EmptyQueue = 0
-- rank (Node l r k v d) = fst d
-- 
-- depth :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> d
-- depth EmptyQueue = 0
-- depth (Node l r k v d) = snd d
-- 
-- toKeyList :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> [k]
-- toKeyList EmptyQueue = []
-- toKeyList (Node l r k v d) = [k] ++ (toKeyList l) ++ (toKeyList r)
-- 
-- readMin :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> v
-- readMin EmptyQueue = error "Reading EmptyQueue"
-- readMin (Node l r k v d) = v
-- 
-- readMinK :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> k
-- readMinK EmptyQueue = error "Reading EmptyQueue"
-- readMinK (Node l r k v d) = k
-- 
-- insert :: (Ord k, Integral d) => k -> v -> PriorityQueue k v (d,d) -> PriorityQueue k v (d,d)
-- insert k' v' EmptyQueue = Node EmptyQueue EmptyQueue k' v' (1,1)
-- insert k' v' (Node l r k v d)
--     |empty l, not (empty r) = error "Insert on improper heap"
--     |rr < rl        = Node oldNode newNode newK newV (newRank,newDepth)
--     |otherwise      = Node newNode oldNode newK newV (newRank,newDepth)
--                 where
--                     rl = rank l
--                     rr = rank r
--                     oldNode = if rr < rl then l else r
--                     replacedNode = if rr < rl then r else l
--                     newNode = if k' < k then insert k v replacedNode else insert k' v' replacedNode
--                     newRank = 1 + (min (rank newNode) (rank replacedNode))
--                     newDepth = 1 + (max (depth newNode) (depth replacedNode))
--                     (newK, newV) = if k' < k then (k',v') else (k,v)
-- 
-- empty EmptyQueue = True
-- empty n = False
-- 
-- --replace with min, and bubble down. Not sure how to do this :( --
-- removeMin :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> PriorityQueue k v (d,d)
-- removeMin EmptyQueue = error "Removing from EmptyQueue"
-- removeMin n@(Node l r k v d) = bubble (popLast (Node l r k' v' d))
--     where
--         bubble EmptyQueue           = EmptyQueue
--         bubble n@(Node l r k v d)
--             | empty l, not (empty r) = error "bubble on improper heap"
--             | empty l               = n
--             | empty r, k <= lk      = n
--             | empty r               = Node (Node EmptyQueue EmptyQueue k v (1,1)) EmptyQueue (readMinK l) (readMin l) (1,2)
--             | k <= lk, k <= rk      = n
--             | lk < rk               = Node (bubble (Node ll lr k v ld)) r lk lv d
--             | otherwise             = Node l (bubble (Node rl rr k v rd)) rk rv d
--             where
--                 getVals (Node l r k v d) = (l,r,k,v,d)
--                 (ll,lr,lk,lv,ld) = getVals l
--                 (rl,rr,rk,rv,rd) = getVals r
--         popLast :: (Ord k, Integral d) => PriorityQueue k v (d,d) -> PriorityQueue k v (d,d)
--         popLast n@(Node l r k v d)
--             | empty l, not (empty r) = error "popLast on improper heap"
--             | empty l           = EmptyQueue
--             | empty r           = Node EmptyQueue EmptyQueue k v (1,1)
--             | depth l > depth r = nextLevel pl r k v
--             | otherwise         = nextLevel l pr k v
--             where 
--                 nextLevel l r k v = Node l r k v (1 + (min (rank l) (rank r)), 1 + (max (depth l) (depth r)))
--                 pl = popLast l
--                 pr = popLast r
--         (k',v') = getLast n
--             where
--                 getLast n@(Node l r k v d)
--                     | empty l, not (empty r) = error "getLast on improper heap"
--                     | empty l            = (k,v)
--                     | empty r            = getLast l
--                     | depth l > depth r  = getLast l
--                     | otherwise          = getLast r
