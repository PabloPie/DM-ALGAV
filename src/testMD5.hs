{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import System.Environment
import Text.Printf

import System.IO

import qualified Data.Vector.Mutable as DVM

import MD5
import Key128
import MinHeapArray as MHA
import ABR as ABR


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

    let (abr, list) = insertAllHashes singlewords
    
    print "#Lear: "
    print (abr)

    print "Liste mots differents"
    print list

    print "Nombre mots differents: "
    print (length list)
    print (countWord list "never")
    

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


invertListAux :: [a] -> [a] -> [a]
invertListAux [] acc = acc
invertListAux (head:tail) acc = invertListAux tail (head:acc)


invertList :: [a] -> [a]
invertList li = invertListAux li []

insertAllHashesAux :: [String] -> ABR.ABR Key128 -> [String] -> (ABR.ABR Key128, [String])
insertAllHashesAux [] abr list = (abr, invertList list)
insertAllHashesAux (head:tail) abr list =
  if ABR.containsABR abr hash then
    insertAllHashesAux tail abr list
  else
    insertAllHashesAux tail (ABR.insertABR abr hash) (head:list)
  where hash = md5 head 


insertAllHashes :: [String] -> (ABR.ABR Key128, [String])
insertAllHashes list = insertAllHashesAux list ABR.ABREmpty []


countWord :: [String] -> String -> Int
countWord [] _ = 0
countWord (head:tail) word =
  if head == word then (countWord tail word) + 1
  else (countWord tail word)
