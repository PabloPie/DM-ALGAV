module Key128
  ( Key,
    inf,
    eq
  ) where

import Data.Word

type Key = (Word64, Word64)

inf :: Key -> Key -> Bool
inf (ax, ay) (bx, by) =
  if ax == bx then ay < by
  else ax < bx

eq :: Key -> Key -> Bool
eq (ax, ay) (bx, by) = ax == bx && ay == by

