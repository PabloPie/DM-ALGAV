{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import System.Environment
import Text.Printf

import System.IO

import qualified Data.Vector.Mutable as DVM

import MD5
import Key128
import MinHeapArray as MHA



main :: IO()
main = do

    -- [filename] <- getArgs

    let list = []
    handle <- openFile "../data/Shakespeare/lear.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents

    -- Step 1: Hasher
    -- Step 2: Inserer hash dans ABR si existe pas
    -- Step 3: Inserer mot non hashe dans la file de priorite si il n'existe pas dans l'ABR
    
    let hashes = md5All singlewords
    heap <- (MHA.consIter hashes)

    print "Mots: "
    print (length singlewords)
    
    print "#Lear: "
    print (DVM.length heap)
    

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
