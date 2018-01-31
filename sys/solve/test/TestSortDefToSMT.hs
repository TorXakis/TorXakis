{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module TestSortDefToSMT
(
testSortDefToSMTList
)
where
import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Text  as T
import           Test.HUnit

import           FuncId
import           SMTData
import           Sort

import           TXS2SMT

testSortDefToSMTList :: Test
testSortDefToSMTList = TestList [
        TestLabel "default"                 testDefaultConstructor
{-        TestLabel "absent Conditional Int"  testAbsentConditionalInt,
        TestLabel "present Condition Int"   testPresentConditionalInt,
        TestLabel "Conditional Int"         testConditionalInt,
        TestLabel "Pair"                    testPair -}
    ]
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testDefaultConstructor :: Test
testDefaultConstructor = TestCase $ assertEqual "default" "" (fst $ adtDefsToSMT Map.empty)

{- testAbsentConditionalInt :: Test
testAbsentConditionalInt = TestCase $ do
    let conditionalIntSortId = SortId "ConditionalInt" 1234
    let absentCstrId    = CstrId "_absent" 2345 [] conditionalIntSortId
    let mapI = EnvNames (Map.fromList [(conditionalIntSortId, "conditionalInt")])
                        (Map.fromList [(absentCstrId, "_absent")])
                        Map.empty
    let envDefs = EnvDefs (Map.fromList [(conditionalIntSortId, SortDef)]) 
                          (Map.fromList [(absentCstrId, CstrDef (FuncId "ignore" 9876 [] conditionalIntSortId) [])])
                          Map.empty
    assertBool "absent" ("(declare-datatypes () (\n    (conditionalInt (_absent))\n) )" `List.isInfixOf ` T.unpack (sortdefsToSMT mapI envDefs) )

testPresentConditionalInt :: Test
testPresentConditionalInt = TestCase $ do
    let conditionalIntSortId = SortId "ConditionalInt" 1234
    let intSortId = SortId "Int" 8765
    let presentCstrId    = CstrId "_present" 2345 [intSortId] conditionalIntSortId
    let valueAccessor = FuncId "value" 6565 [] intSortId
    let mapI = EnvNames (Map.fromList [ (intSortId, "Int")
                                      , (conditionalIntSortId, "conditionalInt")
                                      ])
                        (Map.fromList [(presentCstrId, "_present")])
                        (Map.fromList [(valueAccessor, "value")])
                        
    let envDefs = EnvDefs (Map.fromList [(conditionalIntSortId, SortDef)]) 
                          (Map.fromList [(presentCstrId, CstrDef (FuncId "ignore" 9876 [] conditionalIntSortId) [valueAccessor])])
                          Map.empty
    let result = T.unpack $ sortdefsToSMT mapI envDefs
    assertBool ("present " ++ show result) ("(declare-datatypes () (\n    (conditionalInt (_present (ConditionalInt$_present$0 Int)))\n) )" `List.isInfixOf` result)

testConditionalInt :: Test
testConditionalInt = TestCase $ do
    let conditionalIntSortId = SortId "ConditionalInt" 1234
    let intSortId = SortId "Int" 8765
    let absentCstrId    = CstrId "_absent" 2345 [] conditionalIntSortId
    let presentCstrId    = CstrId "_present" 2345 [intSortId] conditionalIntSortId
    let valueAccessor = FuncId "value" 6565 [] intSortId

    let mapI = EnvNames (Map.fromList [ (intSortId, "Int")
                                      , (conditionalIntSortId, "conditionalInt")
                                      ])
                        (Map.fromList [ (absentCstrId, "_absent")
                                      , (presentCstrId, "_present")
                                      ])
                        (Map.fromList [(valueAccessor, "value")])
                        
    let envDefs = EnvDefs (Map.fromList [(conditionalIntSortId, SortDef)]) 
                          (Map.fromList [(absentCstrId, CstrDef (FuncId "ignore" 9876 [] conditionalIntSortId) [])
                                        ,(presentCstrId, CstrDef (FuncId "ignore" 9876 [] conditionalIntSortId) [valueAccessor])
                                        ])
                          Map.empty
    let result = T.unpack (sortdefsToSMT mapI envDefs)
    assertBool ("conditional " ++ show result) ("(declare-datatypes () (\n    (conditionalInt (_absent) (_present (ConditionalInt$_present$0 Int)))\n) )" `List.isInfixOf` result)

testPair :: Test
testPair = TestCase $ do
    let intSortId = SortId "IntX" 8765
    let pairSortId = SortId "Pair" 1234
    let cstrId    = CstrId "pair" 2345 [] pairSortId
    let firstAccessor = FuncId "first" 6565 [] intSortId
    let secondAccessor = FuncId "second" 6566 [] intSortId

    let mapI = EnvNames (Map.fromList [ (pairSortId, "PairX")
                                      , (intSortId, "IntX")
                                      ])
                        (Map.fromList [ (cstrId, "pairX")
                                      ])
                        (Map.fromList [(firstAccessor, "firstX")
                                      ,(secondAccessor, "secondX")
                                      ])

    let envDefs = EnvDefs (Map.fromList [(pairSortId, SortDef)])
                          (Map.fromList [(cstrId, CstrDef (FuncId "ignore" 9876 [] pairSortId) [firstAccessor, secondAccessor]) ])
                          Map.empty   
    let result = T.unpack (sortdefsToSMT mapI envDefs)
    assertBool ("pair " ++ show result) ("(declare-datatypes () (\n    (PairX (pairX (Pair$pair$0 IntX) (Pair$pair$1 IntX)))\n) )" `List.isInfixOf` result)
-}