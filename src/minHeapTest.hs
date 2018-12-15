{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import System.Clock
import System.Environment
import Formatting.Clock
import Formatting
import System.IO
import Control.Monad
import Data.DoubleWord

import qualified MinHeapTree as T
import qualified MinHeapArray as A

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
    print list
    hClose handle

    start <- getTime Monotonic
    evaluate(T.consIter list)
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end


    start <- getTime Monotonic
    a <- A.consIter list
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end
