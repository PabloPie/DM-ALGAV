module MinHeapArray where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

type MinHeap = M.IOVector

printAll :: (Show e) => MinHeap e -> IO()
printAll vec = do
    if(M.null vec) then
        return ()
    else do
        x <- M.read vec 0
        print x
        printAll (M.tail vec)

-- Create minHeap from 2 minHeaps, vectors are not destroyed in the process
union :: (Ord e) => MinHeap e -> MinHeap e -> IO(MinHeap e)
union vec1 vec2 = do
    -- O(n)
    z <- V.freeze vec1
    -- O(n)
    zz <- V.freeze vec2
    -- O(n+m)
    let v = z V.++ zz
    -- O(n+m)
    new <- constIter $ V.toList v
    return new
    -- M.grow vec1 $ M.length vec2
    -- cpy vec1 vec2 $ M.length vec2
    -- return vec1

-- Add element to vector
add :: (Ord e) => (MinHeap e) -> e -> IO()
add vec ele = do
    M.grow vec 1
    M.write vec (M.length vec -1) ele
    siftUp vec $ M.length vec -1


-- sift the value until the parent is lower or we reach the root
siftUp :: (Ord e) => (MinHeap e) -> Int -> IO()
siftUp vec i = do 
    if i == 0 then
        return ()
    else do
        let parentIndex = div (i-1) 2
        parent <- M.read vec parentIndex
        node <- M.read vec i
        if node < parent then do
            M.swap vec i parentIndex
            siftUp vec parentIndex
        else
            return ()


-- be careful with taking the modified input instead of the output
supprMin :: (Ord e) => (MinHeap e) -> IO(MinHeap e)
supprMin vec = do
    M.swap vec 0 $ M.length vec -1
    let a = M.init vec
    siftDown a 0
    return a

-- Index -> vector size
isLeaf :: Int -> Int -> Bool
isLeaf i n
    | i > n-1 = error "Index out of bounds, not a leaf!"
    | i >= div n 2  = True
    | otherwise = False

-- get the index that contains de minimal value
minIndex :: (Ord e) => (MinHeap e) -> Int -> Int -> IO(Int)
minIndex vec lc rc = do
    -- read only if rc < M.length -1
    if rc < M.length vec then do
        lcval <- M.read vec lc
        rcval <- M.read vec rc
        if lcval < rcval then
            return lc
        else
            return rc
    else
        return lc

-- Sift down the value in index i until a leaf is reached
siftDown :: (Ord e) => (MinHeap e) -> Int -> IO()
siftDown vec i = do
    if isLeaf i $ M.length vec then
        return ()
    else do
        min <- minIndex vec (2*i+1) (2*i+2)
        parent <- M.read vec i
        child <- M.read vec min
        if (parent > child) then do
            M.swap vec i min
            siftDown vec min
        else
            return ()

-- Get the value of the left Child
leftChild :: (MinHeap e) -> Int -> IO(e)
leftChild vec i = do
    M.read vec $ 2*i + 1

-- Get the value of the right Child
rightChild :: (MinHeap e) -> Int -> IO(e)
rightChild vec i = do
    M.read vec $ 2*i + 2

-- heapify subtree starting at index i
heapify :: (Ord e) => (MinHeap e) -> Int -> IO()
heapify vec i = do
    if isLeaf i $ M.length vec then
        return ()
    else do
        heapify vec (i+1)
        siftDown vec i

-- create a vector from a list, make it mutable and heapify it
constIter :: (Ord e) => [e] -> IO(MinHeap e)
constIter li = do
    -- fromList O(n)
    -- unsafeThaw O(1)
    z <- V.unsafeThaw $ V.fromList li
    heapify z 0
    return z

-- cpy :: M.IOVector e -> M.IOVector e -> Int -> IO()
-- cpy vec1 vec2 i = do
--     if M.null vec2 then
--         return ()
--     else do
--         x <- M.read vec2 0
--         M.write vec1 i x
--         let vecaux = M.tail vec2
--         cpy vec1 vecaux (i+1)
