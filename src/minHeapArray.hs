import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

printAll :: (Show e) => M.IOVector e -> IO()
printAll vec = do
    if(M.null vec) then
        return ()
    else do
        x <- M.read vec 0
        print x
        printAll (M.tail vec)

add :: (Ord e) => (M.IOVector e) -> e -> IO(M.IOVector e)
add vec ele = do
    M.grow vec 1
    M.write vec (M.length vec -1) ele
    siftUp vec $ M.length vec -1

siftUp :: (Ord e) => (M.IOVector e) -> Int -> IO(M.IOVector e)
siftUp vec i = do
    if i == 0 then
        return vec
    else do
        let parentIndex = div (i-1) 2
        parent <- M.read vec parentIndex
        node <- M.read vec i
        if node < parent then do
            M.swap vec i parentIndex
            v <- siftUp vec parentIndex
            return v
        else
            return vec


-- be careful with taking the modified input instead of the output
supprMin :: (Ord e) => (M.IOVector e) -> IO(M.IOVector e)
supprMin vec = do
    M.swap vec 0 $ M.length vec -1
    let a = M.init vec
    v <- siftDown a 0
    return v

-- Index -> vector size
isLeaf :: Int -> Int -> Bool
isLeaf i n
    | i > n-1 = error "Index out of bounds, not a leaf!"
    | i >= div n 2  = True
    | otherwise = False

-- get the index that contains de minimal value
minIndex :: (Ord e) => M.IOVector e -> Int -> Int -> IO(Int)
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
siftDown :: (Ord e) => (M.IOVector e) -> Int -> IO(M.IOVector e)
siftDown vec i = do
    if isLeaf i $ M.length vec then
        return vec
    else do
        min <- minIndex vec (2*i+1) (2*i+2)
        parent <- M.read vec i
        child <- M.read vec min
        if (parent > child) then do
            M.swap vec i min
            siftDown vec min
        else
            return vec      

-- Get the value of the left Child
leftChild :: (M.IOVector e) -> Int -> IO(e)
leftChild vec i = do
    M.read vec $ 2*i + 1

-- Get the value of the right Child
rightChild :: (M.IOVector e) -> Int -> IO(e)
rightChild vec i = do
    M.read vec $ 2*i + 2

-- heapify subtree starting at index i
heapify :: (Ord e) => (M.IOVector e) -> Int -> IO(M.IOVector e)
heapify vec i = do
    if isLeaf i $ M.length vec then
        return vec
    else do
        a <- heapify vec (i+1)
        v <- siftDown a i
        return v

constIter :: (Ord e) => [e] -> IO(M.IOVector e)
constIter li = do
    -- fromList O(n)
    -- unsafeThaw O(1)
    z <- V.unsafeThaw $ V.fromList li
    vec <- heapify z 0
    return vec


main = do
    -- v <- M.new 10
    x <- constIter [11,10,9,8,7,6,5,4,3,2,0]
    printAll x
    add x 1
    putStrLn("---")
    printAll x

    -- putStrLn("--------------")
    -- newv <- supprMin v
    -- printAll newv

    -- M.write v 0 (3 :: Int)
    -- M.write v 1 5
    -- M.write v 2 8
    -- a <- leftChild v 0
    -- b <- rightChild v 0
    -- a <- M.read v 0
    -- b <- M.read v 1
    -- print a
    -- print b

    -- print (a<b)
    -- print $ 2 <  M.length v
    
    -- test v

    -- let len = M.length v
    -- x <- M.read v 0
    -- print x
    -- print len

    -- let a = M.tail v
    -- x <- M.read a 0
    -- print x
    -- print $ M.length a
    -- test a
    -- x <- M.read a 0
    -- print x

    -- let b = M.drop 9 a
    -- print $ M.length b
    -- print $ M.null b
