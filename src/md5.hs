import Key128
import Data.Array
import Data.Bits
import Data.Word
import Data.ByteString as DBS (ByteString, unpack)
import Data.ByteString.Char8 as DBC8 (pack)

twoPower32 = 2^32
lookupTableSin = array (0,63) [(i, floor (twoPower32 * (abs (sin (fromInteger(i+1)))))) | i <- [0..63]]



listToArrayAux :: [a] -> Int -> [(Int, a)]
listToArrayAux [] _ = []
listToArrayAux (head:tail) i = ((i),head):(listToArrayAux tail (i+1))

listToArray :: [a] -> Array Int a
listToArray li = array (0, ((length li)-1)) (listToArrayAux li 0)


-- Cree la liste contenant le nombre de decalages binaires a faire pour chaque iteration
shiftList = [7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22, 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20, 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23, 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21]

-- Produit une table permettant de consulter ces decalages en O(1)
shiftTable = listToArray shiftList



a0 = 1732584193
b0 = 4023233417
c0 = 2562383102
d0 = 271733878

word32toInteger :: Word32 -> Integer
word32toInteger = fromIntegral

word32toInt :: Word32 -> Int
word32toInt = fromIntegral


padNChar :: [Word8] -> Int -> [Word8]
padNChar str 0 = str
padNChar str n = padNChar (0:str) (n-1)

pad :: [Word8] -> [Word8]
pad str
  | diff == 0 = str
  | otherwise = padNChar str diff
  where diff = 64-((length str) `mod` 64)




calcFG :: Word32 -> Word32 -> Word32 -> Word32 -> (Word32, Word32)
calcFG i b c d
  | i < 16 = (((b .&. c) .|. ((complement b) .&. d)), i)
  | i < 32 = ((d .&. b) .|. ((complement d) .&. c), ((5*i + 1) `mod` 16))
  | i < 48 = (((b `xor` c) `xor` d), ((3*i + 5) `mod` 16))
  | i < 64 = ((c `xor` (b .|. (complement d))), ((7*i) `mod` 16))



-- F + A + K[i] + M[g]

md5Iter :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Chunk512 -> (Word32, Word32, Word32, Word32)
md5Iter 64 a b c d m = (a0 + a, b0 + b, c0 + c, d0 + d)
md5Iter i a b c d m = md5Iter (i+1) d c b (b + (rotate (f + a + (lookupTableSin!idx) + (m!gdx) + (shiftTable!idx2)) fshift)) m
  where (f, g) = (calcFG i b c d)
        (idx, idx2, gdx, fshift) = ((word32toInteger i), (word32toInt i), (word32toInt f), (word32toInt g))

  
type Chunk512 = Array Int Word32


md5Aux :: [Chunk512] -> Word32 -> Word32 -> Word32 -> Word32 -> (Word32, Word32, Word32, Word32)
md5Aux [] a b c d = (a, b, c, d)
md5Aux (head:tail) a b c d = md5Aux tail a1 b1 c1 d1
  where (a1, b1, c1, d1) = md5Iter 0 a b c d head


toWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
toWord32 a b c d = ((fromIntegral a) `shiftL` 24) .|. ((fromIntegral b) `shiftL` 16) .|. ((fromIntegral c) `shiftL` 8) .|. (fromIntegral d)





word8toWord32 :: [Word8] -> [Word32]
word8toWord32 [] = []
word8toWord32 (a:(b:(c:(d:tail)))) = ((toWord32 a b c d):(word8toWord32 tail))


word32ChunksAux :: [Word32] -> Int -> [Word32] -> ([Word32], [Word32])
word32ChunksAux [] _ carry = (carry, [])
word32ChunksAux (a:tail) i carry
  | i == 0 = (carry, tail)
  | otherwise = word32ChunksAux tail (i-1) (a:carry)


word32toChunks :: [Word32] -> [Chunk512]
word32toChunks [] = []
word32toChunks li = (listToArray head):(word32toChunks tail)
  where (head, tail) = (word32ChunksAux li 16 [])
                       

-- word32toChunks :: [Word32] -> [Chunk512]
-- word32toChunks [] = []
-- word32toChunks (a:(b:(c:(d:tail)))) = (array (0,3) [(0,a), (1,b), (2,c), (3,d)]):(word32toChunks tail)

slice32 :: [Word8] -> [Chunk512]
slice32 str = word32toChunks (word8toWord32 str)

word32to64 :: Word32 -> Word32 -> Word64
word32to64 a b = ((fromIntegral a) `shiftL` 32) .|. (fromIntegral b)

word32to128 :: (Word32, Word32, Word32, Word32) -> Key128
word32to128 (a, b, c, d) = ((word32to64 a b), (word32to64 c d))

strToBS :: String -> DBS.ByteString
strToBS = DBC8.pack

bsToWord8 :: DBS.ByteString -> [Word8]
bsToWord8 = DBS.unpack

md5 :: String -> Key128
md5 str = word32to128 (md5Aux chunks a0 b0 c0 d0)
  where chunks = slice32 (pad (bsToWord8 (strToBS str)))




intToWord32 :: Int -> Word32
intToWord32 = fromIntegral


