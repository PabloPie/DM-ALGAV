import Key128

-- Leftist tree
data BinaryTree a = Empty | Node a Int Int (BinaryTree a) (BinaryTree a) deriving (Read, Show, Eq)

-- Heap are defined as binary trees
data Heap a = BinaryTree a

-- Test function
fac :: Int -> Int
fac 0 = 1
fac n = n*fac (n-1)


heapCons :: a -> BinaryTree a
heapCons elt =  Node elt 0 Empty Empty


-- Return the min value of the heap
heapMin :: BinaryTree a -> a
heapMin (Node r _ _ _) = r


supprLast :: BinaryTree a -> (BinaryTree a, a)
supprLast (Node val _ Empty Empty) = (Empty, val)
supprLast (Node val i (Node lval Empty Empty) Empty) = ((Node val (i-1) Empty Empty), lval)
supprLast (Node val i (Node lval lh llc lrc) (Node rval rh rlc rrc))
| lh == rh = (Node val (i-1)  
| otherwise = (Node val i)

              
heapSupprMin :: (Ord a) => BinaryTree a -> BinaryTree a
heapSupprMin Empty = Empty
heapSupprMin (Node _ lc Empty) = lc
heapSupprMin (Node _ Empty rc) = rc
heapSupprMin (Node _ (Node lval il llc lrc) (Node rval ir rlc rrc))
  | lval  > rval = Node rval (Node lval llc lrc) (heapSupprMin (Node rval rlc rrc))
  | otherwise = Node lval (heapSupprMin (Node lval llc lrc) ) (Node rval rlc rrc)



heapAdd :: (Ord a) => BinaryTree a -> a -> BinaryTree a
heapAdd Empty elt = Node elt Empty Empty
heapAdd (Node val lc rc) elt
  | val < elt = Node val (heapAdd lc elt) rc  
  | otherwise = Node elt (heapAdd lc val) rc


heapConsIterAux :: (Ord a) => BinaryTree a -> [a] -> BinaryTree a
heapConsIterAux tree [] = tree
heapConsIterAux tree (head:tail) = heapConsIterAux (heapAdd tree head) tail

heapConsIter :: (Ord a) => [a] -> BinaryTree a
heapConsIter li = heapConsIterAux Empty li


heapUnion :: (Ord a) => BinaryTree a -> BinaryTree a -> BinaryTree a
heapUnion t1 Empty = t1
heapUnion t1 (Node val lc rc) = heapUnion (heapAdd t1 val) (heapSupprMin (Node val lc rc))


