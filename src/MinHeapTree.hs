module MinHeapTree where


-- Leftist tree
data LeftistTree a = Empty | Node a Int (LeftistTree a) (LeftistTree a) deriving (Read, Show, Eq)

-- Heap are defined as binary trees
type Heap = LeftistTree

-- instance (Show a) => Show (LeftistTree a) where
  -- show heap = show $ traverseBF heap

singleton :: a -> Heap a
singleton x =  Node x 1 Empty Empty

-- Return the min value of the heap
root :: Heap a -> a
root Empty = error "Empty heap!"
root (Node val _ _ _) = val

rank :: Heap a -> Int
rank Empty = 0
rank (Node _ r _ _) = r

-- O(log n)
supprMin :: (Ord a) => Heap a -> Heap a
supprMin Empty = Empty
supprMin (Node _ _ fd fg) = union fg fd

-- O(log n)
add :: (Ord a) => Heap a -> a -> Heap a
add Empty elt = singleton elt
add h1 elt = union h1 $ singleton elt

heapConsIterAux :: (Ord a) => Heap a -> [a] -> Heap a
heapConsIterAux heap [] = heap
heapConsIterAux heap (hd:tl) = heapConsIterAux (add heap hd) tl

consIter :: (Ord a) => [a] -> Heap a
consIter [] = Empty
consIter li = heapConsIterAux Empty li

-- O(log n)
union :: (Ord a) => Heap a -> Heap a -> Heap a
union Empty h2 = h2
union h1 Empty = h1
union h1@(Node rootH1 r1 fg1 fd1) h2@(Node rootH2 r2 fg2 fd2)
    | rootH1 > rootH2 = union h2 h1
    | otherwise =
        if lrank < rrank then (Node rootH1 (r1+1) merged fg1 )
        else (Node rootH1 (rrank+1) fg1 merged )
        where
            lrank = rank fg1
            merged = union fd1 h2
            rrank = rank merged

-- http://matthewmanela.com/blog/breadth-first-tree-traversal-in-haskell/
-- given a Heap, returns a list with its children
subForest :: Heap a -> [Heap a]
subForest (Node _ _ Empty Empty) = []
subForest (Node _ _ Empty b)     = [b]
subForest (Node _ _ a Empty)     = [a]
subForest (Node _ _ a b)         = [a,b]

traverseBF :: Heap a -> [a]
traverseBF Empty = error "Empty heap!"
traverseBF heap = tbf [heap]
    where
        tbf [] = []
        tbf xs = map root xs ++ tbf (concatMap subForest xs)

-- test min Heap property
testHeap :: (Ord a) => Heap a -> Bool
testHeap (Node _ _ Empty Empty) = True
testHeap (Node val _ fg Empty) = val < (root fg) && testHeap (fg)
testHeap (Node val _ Empty fd) = val < (root fd) && testHeap (fd)
testHeap (Node val _ fg fd) =
    (val < (root fg)) && (val < (root fd)) && (testHeap fg) && (testHeap fd)
