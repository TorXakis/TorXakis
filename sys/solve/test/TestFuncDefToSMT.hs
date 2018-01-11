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

import           ConstDefs
import           SMTData
import           SortId
import           TXS2SMT
import           VarId

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
    let envnames = EnvNames Map.empty Map.empty Map.empty
    let envdefs = Map.empty
    assertEqual "none" "" (funcdefsToSMT envnames envdefs)

testConstant :: Test
testConstant = TestCase $ do
    let ve = createVconst (Cint 3)
    let myConst = "myConst"
    let fid = createFunctionId myConst 987654 [] sortRefInt
    let mapI = EnvNames (Map.fromList [(sortRefInt, "Sort_Int")])
                        Map.empty
                        (Map.fromList [(fid,myConst)])
    let (TXS2SMTFuncTest envdefs e) = createFunctionDef mapI fid [] sortRefInt ve
    assertEqual "Constant Function" e (T.unpack (funcdefsToSMT mapI (funcDefs envdefs)))

testSingleArg :: Test
testSingleArg = TestCase $ do
    let v = VarId "x" 645421 sortRefInt
    let ve = createVvar v
    let fName = "singleArgFunction"
    let fid = createFunctionId fName 987654 [v] sortRefInt
    let mapI = EnvNames (Map.fromList [(sortRefInt, "Sort_Int")])
                        Map.empty
                        (Map.fromList [(fid, fName)])
    let (TXS2SMTFuncTest envdefs e) = createFunctionDef mapI fid [v] sortRefInt ve
    assertEqual "single Argument Function" e (T.unpack (funcdefsToSMT mapI (funcDefs envdefs)))

testMultipleArgs :: Test
testMultipleArgs = TestCase $ do
    let varX = VarId "x" 645421 sortRefBool
    let varY = VarId "y" 645422 sortRefBool
    let ve = createVequal (createVvar varX) (createVvar varY)
    let fName = "multipleArgsFunction"
    let fid = createFunctionId fName 987654 [varX, varY] sortRefBool
    let mapI = EnvNames (Map.fromList [(sortRefBool, "SortBoolean")])
                        Map.empty
                        (Map.fromList [(fid, fName)])
    let (TXS2SMTFuncTest envdefs e) = createFunctionDef mapI fid [varX, varY] sortRefBool ve
    assertEqual "multiple Arguments Function" e (T.unpack (funcdefsToSMT mapI (funcDefs envdefs)))

testMultipleFunctions :: Test
testMultipleFunctions = TestCase $ do
    let fName1 = "multipleArgsFunction"
    let varX = VarId "x" 645421 sortRefBool
    let varY = VarId "y" 645422 sortRefBool
    let fid1 = createFunctionId fName1 987654 [varX, varY] sortRefBool
    let vexpr1 = createVequal (createVvar varX) (createVvar varY)

    let fName2 = "myConst"
    let fid2 = createFunctionId fName2 97531 [] sortRefInt
    let vexpr2 = createVconst (Cint 3)

    let mapI = EnvNames (Map.fromList [(sortRefBool, "boolean")
                                      ,(sortRefInt, "integer")
                                      ])
                        Map.empty
                        (Map.fromList [(fid1, fName1)
                                      ,(fid2, fName2)
                                      ])

    let (TXS2SMTFuncTest envdefs expected) = createFunctionDefsRecursive mapI [(fid1,[varX, varY],sortRefBool,vexpr1),(fid2,[],sortRefInt,vexpr2)]

    assertEqual "multiple Functions" expected (T.unpack (funcdefsToSMT mapI (funcDefs envdefs)))
