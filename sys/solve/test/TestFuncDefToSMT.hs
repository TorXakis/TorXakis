{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module TestFuncDefToSMT
(
testFuncDefToSMTList
)
where
import qualified Data.Map           as Map
import qualified Data.Text          as T
import           Test.HUnit

import           StdTDefs
import           TxsDefs

import           TXS2SMT

import           HelperFuncDefToSMT
import           HelperVexprToSMT

-- --------------------------------------------------------------
testFuncDefToSMTList :: Test
testFuncDefToSMTList = TestList [
        TestLabel "none"                testNoFuncDefs,
        TestLabel "const"               testConstant,
        TestLabel "single arg"          testSingleArg,
        TestLabel "multiple args"       testMultipleArgs,
        TestLabel "multiple functions"  testMultipleFunctions
    ]
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testNoFuncDefs :: Test
testNoFuncDefs = TestCase $ do
    let txsDefs = TxsDefs.empty
    assertEqual "none" "" (funcdefsToSMT Map.empty txsDefs)

testConstant :: Test
testConstant = TestCase $ do
    let ve = createVconst (Cint 3)
    let myConst = "myConst"
    let fid = createFunctionId myConst 987654 [] sortId_Int
    let mapI = Map.insert (IdFunc fid) myConst
                (Map.singleton (IdSort sortId_Int) "Sort_Int")
    let (TXS2SMTFuncTest txsdefs e) = createFunctionDef mapI fid [] sortId_Int ve
    assertEqual "Constant Function" e (T.unpack (funcdefsToSMT mapI txsdefs))

testSingleArg :: Test
testSingleArg = TestCase $ do
    let v = VarId "x" 645421 sortId_Int
    let ve = createVvar v
    let fName = "singleArgFunction"
    let fid = createFunctionId fName 987654 [v] sortId_Int
    let mapI = Map.insert (IdFunc fid) fName
                (Map.singleton (IdSort sortId_Int) "Int_Sort")
    let (TXS2SMTFuncTest txsdefs e) = createFunctionDef mapI fid [v] sortId_Int ve
    assertEqual "single Argument Function" e (T.unpack (funcdefsToSMT mapI txsdefs))

testMultipleArgs :: Test
testMultipleArgs = TestCase $ do
    let varX = VarId "x" 645421 sortId_Bool
    let varY = VarId "y" 645422 sortId_Bool
    let ve = createVequal (createVvar varX) (createVvar varY)
    let fName = "multipleArgsFunction"
    let fid = createFunctionId fName 987654 [varX, varY] sortId_Bool
    let mapI = Map.insert (IdFunc fid) fName
               (Map.insert (IdSort sortId_Bool) "SortBoolean"
                Map.empty
               )
    let (TXS2SMTFuncTest txsdefs e) = createFunctionDef mapI fid [varX, varY] sortId_Bool ve
    assertEqual "multiple Arguments Function" e (T.unpack (funcdefsToSMT mapI txsdefs))

testMultipleFunctions :: Test
testMultipleFunctions = TestCase $ do
    let fName1 = "multipleArgsFunction"
    let varX = VarId "x" 645421 sortId_Bool
    let varY = VarId "y" 645422 sortId_Bool
    let fid1 = createFunctionId fName1 987654 [varX, varY] sortId_Bool
    let vexpr1 = createVequal (createVvar varX) (createVvar varY)

    let fName2 = "myConst"
    let fid2 = createFunctionId fName2 97531 [] sortId_Int
    let vexpr2 = createVconst (Cint 3)

    let mapI = Map.insert (IdFunc fid2) fName2
               (Map.insert (IdFunc fid1) fName1
                (Map.insert (IdSort sortId_Bool) "boolean"
                 (Map.insert (IdSort sortId_Int) "integer"
                  Map.empty
               )))

    let (TXS2SMTFuncTest txsdefs expected) = createFunctionDefsRecursive mapI [(fid1,[varX, varY],sortId_Bool,vexpr1),(fid2,[],sortId_Int,vexpr2)]

    assertEqual "multiple Functions" expected (T.unpack (funcdefsToSMT mapI txsdefs))
