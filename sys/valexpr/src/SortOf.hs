{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | SortOf for Value Expressions
module SortOf
( SortOf(..)
)
where

import           SortId

-- | Sort of a value expression
class SortOf s where
  sortOf :: s -> SortId