module Key128
where

import Data.Word
import Text.Printf

type Key128 = (Word64, Word64)



instance Show Key128 where
    show (ax, ay) = (printf "%08x" ax) ++ (printf "%08x" ay)

instance Eq Key128 where
  (ax, ay)==(bx, by) = ax == bx && ay == by


instance Ord Key128 where
  (ax,ay) `compare` (bx,by) = if ax /= bx then compare ax bx else compare ay by

