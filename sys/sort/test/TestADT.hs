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
import Sort

-- ----------------------------------------------------------------------------
testADTList :: Test
testADTList  = TestList [ -- TestLabel "Typed Refs Test" testTypedRef
                        ]

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
-- | Does not compile; type-safety is OK!
-- testTypedRef :: Test
-- testTypedRef = TestCase $ do
--     let r = Ref 12
--         rInt :: TRef Int
--         rInt = TRef r
--         s = SortADT rInt
--         rADT :: TRef ADTDef
--         rADT = TRef r
--         sADT = SortADT rADT
--     assertEqual "Same ADT Reference?" s sADT
