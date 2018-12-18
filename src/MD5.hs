module MD5 (md5) where
import Key128
import Data.Array
import Data.Bits
import Data.Word
import Data.ByteString as DBS (ByteString, unpack)
import Data.ByteString.Char8 as DBC8 (pack)
import Text.Printf


reverseList :: [a] -> [a]
reverseList li = reverseListAux li []

-- twoPower32 = 2^32
-- lookupTableSin = array (0,63) [(i, floor (twoPower32 * (abs (sin (fromInteger(i+1)))))) | i <- [0..63]]

lookupTableSin = listToArray [0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8, 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665, 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391]


reverseListAux :: [a] -> [a] -> [a]
reverseListAux [] carry = carry
reverseListAux (head:tail) carry = reverseListAux tail (head:carry)


listToArrayAux :: [a] -> Int -> [(Int, a)]
listToArrayAux [] _ = []
listToArrayAux (head:tail) i = ((i),head):(listToArrayAux tail (i+1))

listToArray :: [a] -> Array Int a
listToArray li = array (0, ((length li)-1)) (listToArrayAux li 0)


-- Cree la liste contenant le nombre de decalages binaires a faire pour chaque iteration
shiftList = [7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22, 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20, 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23, 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21]

-- Produit une table permettant de consulter ces decalages en O(1)
shiftTable = listToArray shiftList



a0 = 0x67452301
b0 = 0xefcdab89
c0 = 0x98badcfe
d0 = 0x10325476

word32toInteger :: Word32 -> Integer
word32toInteger = fromIntegral

word32toInt :: Word32 -> Int
word32toInt = fromIntegral


pad512 :: [Word8] -> Int -> [Word8]
pad512 str len
  | zeros == 0 = str
  | otherwise = str ++ (take zeros (cycle [0]))
  where zeros = (64-(len+1+8) `mod` 64)


word32to8 :: Word32 -> (Word8, Word8, Word8, Word8)
word32to8 val = (fromIntegral ((val .&. 0xFF000000 :: Word32) `shiftR` 24),
                 fromIntegral ((val .&. 0x00FF0000 :: Word32) `shiftR` 16),
                 fromIntegral ((val .&. 0x0000FF00 :: Word32) `shiftR` 8),
                 fromIntegral (val .&. 0x000000FF :: Word32))

appendDWord :: [Word8] -> Word32 -> [Word8]
appendDWord li w = li ++ [d, c, b, a, 0, 0, 0, 0]
  where (a, b, c, d) = word32to8 w

appendByte :: [Word8] -> Word8 -> [Word8]
appendByte li b = li ++ [b]

prepare :: [Word8] -> [Word8]
prepare li = appendDWord padded (intToWord32 ((length li)*8))
  where padded = pad512 (appendByte li 0x80) (length li)

--        if 0 ≤ i ≤ 15 then
--            F := (B and C) or ((not B) and D)
--            g := i
--        else if 16 ≤ i ≤ 31 then
--            F := (D and B) or ((not D) and C)
--            g := (5×i + 1) mod 16
--       else if 32 ≤ i ≤ 47 then
--            F := B xor C xor D
--            g := (3×i + 5) mod 16
--        else if 48 ≤ i ≤ 63 then
--            F := C xor (B or (not D))
--           g := (7×i) mod 16

calcFG :: Word32 -> Word32 -> Word32 -> Word32 -> (Word32, Word32)
calcFG i b c d
  | i < 16 = (((b .&. c) .|. ((complement b) .&. d)), i)
  | i < 32 = ((d .&. b) .|. ((complement d) .&. c), ((5*i + 1) `mod` 16))
  | i < 48 = (((b `xor` c) `xor` d), ((3*i + 5) `mod` 16))
  | i < 64 = ((c `xor` (b .|. (complement d))), ((7*i) `mod` 16))



-- F + A + K[i] + M[g]

--        F := F + A + K[i] + M[g]
--        A := D
--        D := C
--        C := B
--        B := B + leftrotate(F, s[i])

--md5Iter i a b c d m = md5Iter (i+1) d c b (b + (rotate (f + a + (lookupTableSin!idx) + (m!gdx) + (shiftTable!idx2)) fshift)) m

lshift :: Word32 -> Int -> Word32
lshift val s = rotate val s


md5Iter :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Chunk512 -> [String] -> (Word32, Word32, Word32, Word32, [String])
md5Iter 64 a b c d m strl = (a0 + a, b0 + b, c0 + c, d0 + d, strl)
md5Iter i a b c d m strl = md5Iter (i+1) a1 b1 c1 d1 m (str1:strl)
  where (f, g) = (calcFG i b c d)
        (idx, idx2, gdx) = ((word32toInt i), (word32toInt i), (word32toInt g))
        (a1, b1, c1, d1) = (d, (b + lshift (f + a + (lookupTableSin!idx) + (m!gdx)) (shiftTable!idx2)), b, c)
        str1 = (printf "rotateLeft(%08x + %08x + %08x + %08x (%d), %08x - %08x) \n" a f (lookupTableSin!idx) (m!gdx) gdx (m!0) (shiftTable!idx))


  
type Chunk512 = Array Int Word32


md5Aux :: [Chunk512] -> Word32 -> Word32 -> Word32 -> Word32 -> [String] -> (Word32, Word32, Word32, Word32, [String])
md5Aux [] a b c d strl = (a, b, c, d, strl)
md5Aux (head:tail) a b c d strl  = md5Aux tail a1 b1 c1 d1 strl1
  where (a1, b1, c1, d1, strl1) = md5Iter 0 a b c d head ("      ":strl)


toWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
toWord32 a b c d = ((fromIntegral d) `shiftL` 24) .|. ((fromIntegral c) `shiftL` 16) .|. ((fromIntegral b) `shiftL` 8) .|. (fromIntegral a)





word8toWord32 :: [Word8] -> [Word32]
word8toWord32 [] = []
word8toWord32 (a:(b:(c:(d:tail)))) = ((toWord32 a b c d):(word8toWord32 tail))


-- word32ChunksAux :: [Word32] -> Int -> [Word32] -> ([Word32], [Word32])
-- word32ChunksAux [] _ carry = (carry, [])
-- word32ChunksAux (a:tail) i carry
--  | i == 0 = (carry, tail)
--  | otherwise = word32ChunksAux tail (i-1) (a:carry)


-- word32toChunks :: [Word32] -> [Chunk512]
-- word32toChunks [] = []
-- word32toChunks li = (listToArray head):(word32toChunks tail)
--   where (head, tail) = (word32ChunksAux li 16 [])


word32ChunksAux :: [Word32] -> Int -> [Word32] -> [Chunk512] -> [Chunk512]
word32ChunksAux [] 0 [] carry = carry
word32ChunksAux [] 0 curList carry = (listToArray (reverseList curList)):carry
word32ChunksAux li 0 curList carry = (word32ChunksAux li 16 [] ((listToArray (reverseList curList)):carry))
word32ChunksAux (head:tail) i curList carry = word32ChunksAux tail (i-1) (head:curList) carry

word32toChunks :: [Word32] -> [Chunk512]
word32toChunks li = word32ChunksAux li 16 [] []


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



invertBytes :: Word32 -> Word32
invertBytes w = toWord32 a b c d
  where (a, b, c, d) = word32to8 w
          


md5 :: String -> (Key128)
md5 str = word32to128 (invertBytes h1, invertBytes h2, invertBytes h3, invertBytes h4)
  where chunks = slice32 (prepare (bsToWord8 (strToBS str)))
        (h1, h2, h3, h4, strl) = md5Aux chunks a0 b0 c0 d0 []

md5W32 :: [Word32] -> Key128
md5W32 w = word32to128 (a, b, c, d)
  where (a, b, c, d, strl) = (md5Aux (word32toChunks w) a0 b0 c0 d0 [])


intToWord32 :: Int -> Word32
intToWord32 = fromIntegral


showArrayAux :: Array Integer Word32 -> Int -> String
showArrayAux arr 0 = ""
showArrayAux arr i = (showArrayAux arr (i-1)) ++ (printf "%08x, " (arr!(fromIntegral i))) 


showArray :: Array Integer Word32 -> String
showArray arr =  showArrayAux arr ((length arr)-1) 
