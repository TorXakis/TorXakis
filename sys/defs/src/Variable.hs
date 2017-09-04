{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Variable

where

import           Name
import           SortId

class (Eq v, Ord v, Read v, Show v) => Variable v where
  vname  :: v -> Name
  vunid  :: v -> Int
  vsort  :: v -> SortId
  cstrVariable  :: String -> Int -> SortId -> v
