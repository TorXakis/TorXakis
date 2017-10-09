{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestPreGNF
(
testPreGNFList
)
where
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map

-- import Debug.Trace as Trace

import TxsDefs
import TxsShow

import PreGNF

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

-- Stop
testStop :: Test
testStop = TestCase $
    let bexpr :: BExpr
        bexpr = Stop
      in
        assertEqual "STOP" (preGNF 1) 2

----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testPreGNFList :: Test
testPreGNFList = TestList [ TestLabel "Stop" testStop
                         ]