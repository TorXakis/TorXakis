{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings      #-}
module TestADT
(
testADTList
)
where
-- test specific Haskell imports
import Test.HUnit

-- test specific TorXakis imports

-- generic Haskell imports

-- generic TorXakis imports
import Ref

-- ----------------------------------------------------------------------------
testADTList :: Test
testADTList  = TestList [ TestLabel "Ref Test" testRef
                        ]

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testRef :: Test
testRef = TestCase $ do
    let expected = 12
        rInt :: Ref Int
        rInt = Ref expected
    assertEqual "Same reference?" expected $ toInt rInt
