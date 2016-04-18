-- Priority Queue Data Structure
-- Author: David Ackerman
-- Apr 17, 2016

module PriorityQueue (PriorityQueue(EmptyQueue,Leaf,Branch,Node),peep,pop,insert,depth,toList,merge) where

data PriorityQueue k v = 
          EmptyQueue
        | Leaf k v 
        | Branch (PriorityQueue k v) k v 
        | Node (PriorityQueue k v) (PriorityQueue k v) k v

peep :: Ord k => PriorityQueue k v -> v
peep EmptyQueue = error "Peeping into empty Queue!"
peep (Leaf k v) = v
peep (Branch _ k v) = v
peep (Node _ _ k v) = v

depth :: Ord k => PriorityQueue k v -> Int
depth EmptyQueue = 0
depth (Leaf k v) = 1
depth (Branch l k v) = 1 + depth l
depth (Node l r k v) = 1 + depth l

toList :: Ord k => PriorityQueue k v -> [v]
toList EmptyQueue = []
toList (Leaf k v) = [v]
toList (Branch l k v) = [v] ++ toList l
toList (Node l r k v) = [v] ++ toList l ++ toList r

insert :: Ord k => k -> v -> PriorityQueue k v -> PriorityQueue k v
insert xk x EmptyQueue = Leaf xk x
insert xk x (Leaf k v) 
    | xk < k       = Branch (Leaf k v) xk x
    | otherwise   = Branch (Leaf xk x) k v
insert xk x (Branch l k v)
    | xk < k       = Node l (Leaf k v) xk x
    | otherwise   = Node l (Leaf xk x) k v
insert xk x (Node l r k v)
    | depth l > depth r = if k > xk then Node l (insert k v r) xk x
                        else Node l (insert xk x r) k v
    | otherwise         = if k > xk then Node (insert k v l) r xk x
                        else Node (insert xk x l) r k v

pop :: Ord k => PriorityQueue k v -> PriorityQueue k v
pop EmptyQueue = error "Popping empty Queue"
pop (Leaf k v) = EmptyQueue
pop (Branch l k v) = l
pop (Node l r k v) = merge l r

merge :: Ord k => PriorityQueue k v -> PriorityQueue k v -> PriorityQueue k v
merge nL EmptyQueue = nL
merge nL (Leaf k v) = insert k v nL
merge nL (Branch l k v) = insert k v (merge nL l)
merge nL (Node l r k v) = insert k v (merge nL (merge l r))
