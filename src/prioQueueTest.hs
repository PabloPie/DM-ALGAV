{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import System.Clock
import System.Environment
import Formatting.Clock
import Formatting
import System.IO
import Control.Monad
import Data.DoubleWord
import Data.Bits

import qualified MinHeapTree as T
import qualified MinHeapArray as A
import qualified FileBinomiale as F
import qualified Data.Heap as D
import qualified Data.Vector as V

f :: [String] -> [Word128]
f = map read

-- give the file location as program argument, e.g. ../data/jeu_1_nb_cles_100.txt
-- if given no arguments or more than 1, it won't show a comprehensible error
main :: IO()
main = do

    [filename] <- getArgs

    let list = []
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = f singlewords

    -- testHeap list
    benchUnion list
    testQueue list

testHeap :: [Word128] -> IO()
testHeap list = do
    putStr ("Tree: ")
    let a = T.consIter list
    print $ T.testHeap a
    putStr ("Array: ")
    a <- A.consIter list
    z <- V.unsafeFreeze a
    print $ A.testHeap z 0

testQueue :: [Word128] -> IO()
testQueue list = do
    let a = F.consIter list
    print $ popCount (length list) == length a

benchCons :: [Word128] -> IO()
benchCons list = do
    putStr "Tree minHeap consIter: "
    start <- getTime Monotonic
    evaluate(T.consIter list)
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end

    putStr "Array minHeap consIter: "
    start <- getTime Monotonic
    a <- A.consIter list
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end

    putStr "Haskell Data.Heap fromList: "
    start <- getTime Monotonic
    evaluate(D.fromList list :: D.MinHeap Word128)
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end

    putStr "Binomial Heap consIter: "
    start <- getTime Monotonic
    evaluate(F.consIter list)
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end

benchUnion :: [Word128] -> IO() 
benchUnion list = do
    putStr "Tree minHeap union: "
    let (l1,l2) = splitAt (length list `div` 2) list
    let a = T.consIter l1
    let b = T.consIter l2
    start <- getTime Monotonic
    evaluate(T.union a b)
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end
    let c = T.union a b

    putStr "Array minHeap union: "
    a <- A.consIter l1
    b <- A.consIter l2
    start <- getTime Monotonic
    c <- A.union a b
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end

    putStr "Binomial Heap union: "
    let a = F.consIter l1
    let b = F.consIter l2
    start <- getTime Monotonic
    evaluate(F.union a b)
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end 

benchAdd :: [Word128] -> IO()
benchAdd list = do
    putStr "Tree minHeap add: "
    let a = T.consIter list
    start <- getTime Monotonic
    evaluate(T.add a 1)
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end

    putStr "Array minHeap add: "
    a <- A.consIter list
    start <- getTime Monotonic
    A.add a 1
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end
