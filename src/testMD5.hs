{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import System.Environment
import Text.Printf

import System.IO

import MD5
import Key128
import MinHeapArray as MHA



main :: IO()
main = do

    --[filename] <- getArgs

    let list = []
    handle <- openFile "../data/Shakespeare/lear.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents

    let hashes = md5All singlewords
    let tree = (MHA.consIter hashes)

    putStr "end"
    

md5AllAux :: [String] -> [Key128] -> [Key128]
md5AllAux [] acc = acc
md5AllAux (head:tail) acc = md5AllAux tail ((md5 head):acc)
       
md5All :: [String] -> [Key128]
md5All [] = []
md5All (head:tail) = md5AllAux (head:tail) []

printAllData :: [Key128] -> IO()
printAllData [] = do
  putStr ""
printAllData (head:tail) = do
  putStr (" " ++ (show head))
  printAllData tail
