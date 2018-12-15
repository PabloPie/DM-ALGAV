{-# LANGUAGE OverloadedStrings #-}
import qualified MinHeapTree as T
import qualified MinHeapArray as A
import Control.Exception
import System.Clock
import Formatting.Clock
import Formatting

main :: IO()
main = do
    lastVal <- getLine
    let x = (read lastVal :: Int)
    start <- getTime Monotonic
    evaluate(T.consIter [0,1..x])
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end

    start <- getTime Monotonic
    a <- A.consIter [0,1..x]
    -- b <- A.consIter [20,19..11]
    -- c <- A.union a b
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end

    -- A.printAll c


