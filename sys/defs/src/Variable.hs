{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module Variable

where
import SortId

class (Eq v, Ord v, Read v, Show v) => Variable v
  where
    vname  :: v -> String
    vunid  :: v -> Int
    vsort  :: v -> SortId
    cstrVariable  :: String -> Int -> SortId -> v

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
