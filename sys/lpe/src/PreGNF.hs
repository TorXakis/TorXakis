{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module PreGNF
( preGNF )

-- ----------------------------------------------------------------------------------------- --
-- import

where

--import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set

-- import from local
--import           Next
--import           Reduce
--import           Unfold

-- import from defs
import qualified TxsDefs
import qualified TxsUtils


-- ----------------------------------------------------------------------------------------- --
-- preGNF :  ...
--               :  result list is empty: refusal is not possible
--               :  result contains empty list: STOP is possible after refusal

preGNF :: Int -> Int
preGNF 0 = 0
preGNF n = n+1

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
