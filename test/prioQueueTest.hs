{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import System.Clock
import System.Environment
import Formatting.Clock
import Formatting
import System.IO
import qualified System.IO.Strict as S
import Control.Monad
import Data.DoubleWord
import Data.Bits
import Control.DeepSeq
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import qualified MinHeapTree as T
import qualified MinHeapArray as A
import qualified FileBinomiale as F
import qualified Data.Heap as D
import qualified Data.Vector as V

f :: [String] -> [Word128]
f = map read

average :: [Int] -> Int
average xs = sum xs `div` length xs

-- argument is size of text
main :: IO()
main = do

    [size] <- getArgs
    c1 <- S.readFile $ "../data/jeu_1_nb_cles_"++size++".txt"
    c2 <- S.readFile $ "../data/jeu_2_nb_cles_"++size++".txt"
    c3 <- S.readFile $ "../data/jeu_3_nb_cles_"++size++".txt"
    c4 <- S.readFile $ "../data/jeu_4_nb_cles_"++size++".txt"
    c5 <- S.readFile $ "../data/jeu_5_nb_cles_"++size++".txt"
    let contents = [c1,c2,c3,c4,c5]
    let singlewords = map words contents
    let lists@[l1,l2,l3,l4,l5] = map f singlewords
    print lists

    putStrLn "consIter (ns)"
    putStrLn "---"
    time1@[h1,h2,h3,h4,h5] <- mapM benchConsHeap lists
    time1@[h1,h2,h3,h4,h5] <- mapM benchConsHeap lists
    time2@[t1,t2,t3,t4,t5] <- mapM benchConsTree lists
    time3@[a1,a2,a3,a4,a5] <- mapM benchConsArray lists
    time4@[q1,q2,q3,q4,q5] <- mapM benchConsQueue lists

    putStrLn "Data.Heap:"
    print $ average $ map fromIntegral time1
    putStrLn "Tree:"
    print $ average $ map fromIntegral time2
    putStrLn "Array:"
    print $ average $ map fromIntegral time3
    putStrLn "Queue:"
    print $ average $ map fromIntegral time4

    putStrLn ""
    putStrLn "union (ns)"
    putStrLn "---"
    time1 <- benchUnionTree l1 l2
    time11 <- benchUnionTree l3 l5
    time2 <- benchUnionArray l1 l2
    time21 <- benchUnionArray l3 l5
    time3 <- benchUnionQueue l1 l2
    time31 <- benchUnionQueue l3 l5

    putStrLn "Tree:"
    print time1
    print time11
    putStrLn "Array:"
    print time2
    print time21
    putStrLn "Queue:"
    print time3
    print time31

    putStrLn ""
    putStrLn "add (ns)"
    putStrLn "---"
    time1 <- benchAddTree l1 1
    time11 <- benchAddTree l2 1
    time2 <- benchAddArray l1 1
    time21 <- benchAddArray l2 1
    time3 <- benchAddQueue l1 1
    time31 <- benchAddQueue l2 1

    putStrLn "Tree:"
    print time1
    print time11
    putStrLn "Array:"
    print time2
    print time21
    putStrLn "Queue:"
    print time3
    print time31

    putStrLn ""
    putStrLn "supprMin (ns)"
    putStrLn "---"
    time1 <- benchSupTree l1
    time11 <- benchSupTree l2
    time2 <- benchSupArray l1
    time21 <- benchSupArray l2
    time3 <- benchSupQueue l1
    time31 <- benchSupQueue l2

    putStrLn "Tree:"
    print time1
    print time11
    putStrLn "Array:"
    print time2
    print time21
    putStrLn "Queue:"
    print time3
    print time31

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
    putStr $ "Binomial Queue: "
    print $ popCount (length list) == length a

--- Time consIter

benchConsHeap :: [Word128] -> IO Integer
benchConsHeap list = do
    start <- getTime Monotonic
    evaluate(D.fromList list :: D.MinHeap Word128)
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

benchConsTree :: [Word128] -> IO Integer
benchConsTree list = do
    start <- getTime Monotonic
    evaluate(T.consIter list)
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

benchConsArray :: [Word128] -> IO Integer
benchConsArray list = do
    start <- getTime Monotonic
    A.consIter list
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

benchConsQueue :: [Word128] -> IO Integer
benchConsQueue list = do
    start <- getTime Monotonic
    evaluate(F.consIter list)
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

--- Time Union

benchUnionTree :: [Word128] -> [Word128] -> IO Integer
benchUnionTree l1 l2 = do
    let a = T.consIter l1
    let b = T.consIter l2 
    start <- getTime Monotonic
    evaluate(T.unionAlt a b)
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

benchUnionArray :: [Word128] -> [Word128] -> IO Integer
benchUnionArray l1 l2 = do
    a <- A.consIter l1
    b <- A.consIter l2 
    start <- getTime Monotonic
    A.union a b 
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

benchUnionQueue :: [Word128] -> [Word128] -> IO Integer
benchUnionQueue l1 l2 = do
    let a = F.consIter l1
    let b = F.consIter l2 
    start <- getTime Monotonic
    evaluate(F.union a b)
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)


--- Time add

benchAddTree :: [Word128] -> Word128 -> IO Integer
benchAddTree l1 ele = do
    let a = T.consIter l1
    start <- getTime Monotonic
    evaluate(T.add a ele)
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

benchAddArray :: [Word128] -> Word128 -> IO Integer
benchAddArray l1 ele = do
    a <- A.consIter l1
    start <- getTime Monotonic
    A.add a ele  
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

benchAddQueue :: [Word128] -> Word128 -> IO Integer
benchAddQueue l1 ele = do
    let a = F.consIter l1
    start <- getTime Monotonic
    evaluate(F.ajoutElem a ele)
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

-----------

--- Time supprMin

benchSupTree :: [Word128] -> IO Integer
benchSupTree l1 = do
    let a = T.consIter l1
    start <- getTime Monotonic
    evaluate(T.supprMin a)
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

benchSupArray :: [Word128] -> IO Integer
benchSupArray l1 = do
    a <- A.consIter l1
    start <- getTime Monotonic
    b <- A.supprMin a
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)

benchSupQueue :: [Word128] -> IO Integer
benchSupQueue l1 = do
    let a = F.consIter l1
    start <- getTime Monotonic
    evaluate(F.supprMin a)
    end <- getTime Monotonic
    return (toNanoSecs end - toNanoSecs start)


benchCons :: [Word128] -> IO()
benchCons list = do

    putStr "Haskell Data.Heap fromList: "
    start <- getTime Monotonic
    evaluate(D.fromList list :: D.MinHeap Word128)
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end

    putStr "Array minHeap consIter: "
    start <- getTime Monotonic
    A.consIter list
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end 

    putStr "Tree minHeap consIter: "
    start <- getTime Monotonic
    evaluate(T.consIter list)
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
    A.union a b
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
