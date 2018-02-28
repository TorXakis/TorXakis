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
import           Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import           Test.HUnit

import           ConstDefs
import           Name
import           Sort
import           TXS2SMT
import           VarId

import           HelperFuncDefToSMT
import           HelperVexprToSMT

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
testNoFuncDefs = TestCase $ assertEqual "none" "" (funcdefsToSMT Map.empty)

testConstant :: Test
testConstant = TestCase $ do
    let ve = createVconst (Cint 3)
    let myConst = fromNonEmpty $ fromList "myConst"
    let fid = createFunctionId myConst 987654 [] SortInt
    let (TXS2SMTFuncTest i e) = createFunctionDef fid [] SortInt ve
    assertEqual "Constant Function" e (funcdefsToSMT i)

testSingleArg :: Test
testSingleArg = TestCase $ do
    let v = VarId (fromNonEmpty $ fromList "x") 645421 SortInt
    let ve = createVvar v
    let fName = fromNonEmpty $ fromList "singleArgFunction"
    let fid = createFunctionId fName 987654 [v] SortInt
    let (TXS2SMTFuncTest i e) = createFunctionDef fid [v] SortInt ve
    assertEqual "single Argument Function" e (funcdefsToSMT i)

testMultipleArgs :: Test
testMultipleArgs = TestCase $ do
    let varX = VarId (fromNonEmpty $ fromList "x") 645421 SortBool
    let varY = VarId (fromNonEmpty $ fromList "y") 645422 SortBool
    let ve = createVequal (createVvar varX) (createVvar varY)
    let fName = fromNonEmpty $ fromList "multipleArgsFunction"
    let fid = createFunctionId fName 987654 [varX, varY] SortBool
    let (TXS2SMTFuncTest i e) = createFunctionDef fid [varX, varY] SortBool ve
    assertEqual "multiple Arguments Function" e (funcdefsToSMT i)

testMultipleFunctions :: Test
testMultipleFunctions = TestCase $ do
    let fName1 = fromNonEmpty $ fromList "multipleArgsFunction"
    let varX = VarId (fromNonEmpty $ fromList "x") 645421 SortBool
    let varY = VarId (fromNonEmpty $ fromList "y") 645422 SortBool
    let fid1 = createFunctionId fName1 987654 [varX, varY] SortBool
    let vexpr1 = createVequal (createVvar varX) (createVvar varY)

    let fName2 = fromNonEmpty $ fromList "myConst"
    let fid2 = createFunctionId fName2 97531 [] SortInt
    let vexpr2 = createVconst (Cint 3)
    let (TXS2SMTFuncTest i expected) = createFunctionDefsRecursive [(fid1,[varX, varY],SortBool,vexpr1),(fid2,[],SortInt,vexpr2)]
    assertEqual "multiple Functions" expected (funcdefsToSMT i)
