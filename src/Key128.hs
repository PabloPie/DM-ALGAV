module Key128
  ( Key128,
    inf,
    eq
  ) where

import Data.Word
import Text.Printf

type Key128 = (Word64, Word64)



instance Show Key128 where
    show (ax, ay) = (printf "%08x" ax) ++ (printf "%08x" ay)



inf :: Key128 -> Key128 -> Bool
inf (ax, ay) (bx, by) =
  if ax == bx then ay < by
  else ax < bx

eq :: Key128 -> Key128 -> Bool
eq (ax, ay) (bx, by) = ax == bx && ay == by


consKey128 :: Word64 -> Word64 -> Key128
consKey128 a b = (a, b)
