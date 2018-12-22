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

import qualified MinHeapTree as T
import qualified MinHeapArray as A
import qualified FileBinomiale as F
import qualified Data.Heap as D
import qualified Data.Vector as V
import System.Random (randomRIO)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,18446744073709551614) :: IO Word64
  rs <- randomList (n-1)
  return (r:rs)


-- argument is size of text
main :: IO()
main = do
    [x] <- getArgs
    let nb = read x :: Int
    c <- randomList nb
    
    start <- getTime Monotonic
    evaluate $ F.consIter c
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end
