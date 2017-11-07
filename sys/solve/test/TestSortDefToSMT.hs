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

import           TxsDefs

import           TXS2SMT

-- --------------------------------------------------------------
testSortDefToSMTList :: Test
testSortDefToSMTList = TestList [
        TestLabel "default"                 testDefaultConstructor,
        TestLabel "absent Conditional Int"  testAbsentConditionalInt,
        TestLabel "present Condition Int"   testPresentConditionalInt,
        TestLabel "Conditional Int"         testConditionalInt,
        TestLabel "Pair"                    testPair
    ]
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testDefaultConstructor :: Test
testDefaultConstructor = TestCase $ assertEqual "default" "" (sortdefsToSMT Map.empty TxsDefs.empty)

testAbsentConditionalInt :: Test
testAbsentConditionalInt = TestCase $ do
    let conditionalIntSortId = SortId "ConditionalInt" 1234
    let absentCstrId    = CstrId "_absent" 2345 [] conditionalIntSortId
    let mapI = Map.insert (IdCstr absentCstrId) "_absent"
               (Map.insert (IdSort conditionalIntSortId) "conditionalInt"
                Map.empty
               )
    let txsDefs = TxsDefs.insert (IdCstr absentCstrId) (DefCstr (CstrDef (FuncId "ignore" 9876 [] conditionalIntSortId) []) )
                 (TxsDefs.insert (IdSort conditionalIntSortId) (DefSort SortDef)
                  TxsDefs.empty)
    assertBool "absent" ("(declare-datatypes () (\n    (conditionalInt (_absent))\n) )" `List.isInfixOf ` T.unpack (sortdefsToSMT mapI txsDefs) )

testPresentConditionalInt :: Test
testPresentConditionalInt = TestCase $ do
    let conditionalIntSortId = SortId "ConditionalInt" 1234
    let intSortId = SortId "Int" 8765
    let presentCstrId    = CstrId "_present" 2345 [intSortId] conditionalIntSortId
    let valueAccessor = FuncId "value" 6565 [] intSortId
    let mapI = Map.insert (IdCstr presentCstrId) "_present"
               (Map.insert (IdSort conditionalIntSortId) "conditionalInt"
                (Map.insert (IdSort intSortId) "Int"
                 (Map.insert (IdFunc valueAccessor) "value"
                  Map.empty
               )))
    let txsDefs = TxsDefs.insert (IdCstr presentCstrId) (DefCstr (CstrDef (FuncId "ignore" 9876 [] conditionalIntSortId) [valueAccessor]) )
                  (TxsDefs.insert (IdSort conditionalIntSortId) (DefSort SortDef)
                   TxsDefs.empty)
    let result = T.unpack $ sortdefsToSMT mapI txsDefs
    assertBool ("present " ++ show result) ("(declare-datatypes () (\n    (conditionalInt (_present (ConditionalInt$_present$0 Int)))\n) )" `List.isInfixOf` result)

testConditionalInt :: Test
testConditionalInt = TestCase $ do
    let conditionalIntSortId = SortId "ConditionalInt" 1234
    let intSortId = SortId "Int" 8765
    let absentCstrId    = CstrId "_absent" 2345 [] conditionalIntSortId
    let presentCstrId    = CstrId "_present" 2345 [intSortId] conditionalIntSortId
    let valueAccessor = FuncId "value" 6565 [] intSortId
    let mapI = Map.insert (IdCstr absentCstrId) "_absent"
               (Map.insert (IdCstr presentCstrId) "_present"
                (Map.insert (IdSort conditionalIntSortId) "conditionalInt"
                 (Map.insert (IdSort intSortId) "Int"
                  (Map.insert (IdFunc valueAccessor) "value"
                   Map.empty
               ))))
    let txsDefs = TxsDefs.insert (IdCstr absentCstrId) (DefCstr (CstrDef (FuncId "ignore" 9876 [] conditionalIntSortId) []) )
                  (TxsDefs.insert (IdCstr presentCstrId) (DefCstr (CstrDef (FuncId "ignore" 9876 [] conditionalIntSortId) [valueAccessor]) )
                   (TxsDefs.insert (IdSort conditionalIntSortId) (DefSort SortDef)
                    TxsDefs.empty))
    let result = T.unpack (sortdefsToSMT mapI txsDefs)
    assertBool ("conditional " ++ show result) ("(declare-datatypes () (\n    (conditionalInt (_absent) (_present (ConditionalInt$_present$0 Int)))\n) )" `List.isInfixOf` result)

testPair :: Test
testPair = TestCase $ do
    let intSortId = SortId "IntX" 8765
    let pairSortId = SortId "Pair" 1234
    let cstrId    = CstrId "pair" 2345 [] pairSortId
    let firstAccessor = FuncId "first" 6565 [] intSortId
    let secondAccessor = FuncId "second" 6566 [] intSortId

    let mapI = Map.insert (IdSort pairSortId) "PairX"
               (Map.insert (IdCstr cstrId) "pairX"
                (Map.insert (IdFunc firstAccessor) "firstX"
                 (Map.insert (IdSort intSortId) "IntX"
                  (Map.insert (IdFunc secondAccessor) "secondX"
                   Map.empty
               ))))

    let txsDefs = TxsDefs.insert (IdCstr cstrId) (DefCstr (CstrDef (FuncId "ignore" 9876 [] pairSortId) [firstAccessor, secondAccessor]) )
                  (TxsDefs.insert (IdSort pairSortId) (DefSort SortDef)
                   TxsDefs.empty)
    let result = T.unpack (sortdefsToSMT mapI txsDefs)
    assertBool ("pair " ++ show result) ("(declare-datatypes () (\n    (PairX (pairX (Pair$pair$0 IntX) (Pair$pair$1 IntX)))\n) )" `List.isInfixOf` result)
