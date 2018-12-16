module Key128
  ( Key128,
    inf,
    eq
  ) where

import Data.Word
import Numeric (showHex)

type Key128 = (Word64, Word64)


instance Show Key128 where
  show (ax, ay) = (showHex ax "") ++ (showHex ay "")



inf :: Key128 -> Key128 -> Bool
inf (ax, ay) (bx, by) =
  if ax == bx then ay < by
  else ax < bx

eq :: Key128 -> Key128 -> Bool
eq (ax, ay) (bx, by) = ax == bx && ay == by


