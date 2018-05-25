{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module TestFuncDefToSMT
(
testFuncDefToSMTList
)
where
import qualified Data.Map           as Map
import qualified Data.Text          as T
import           Test.HUnit

import           ConstDefs
import           FuncDef(FuncDef)
import           FuncId(FuncId)
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
        envdefs :: Map.Map FuncId (FuncDef VarId)
        envdefs = Map.empty
    assertEqual "none" "" (funcdefsToSMT envnames envdefs)

testConstant :: Test
testConstant = TestCase $ do
    let ve = createVconst (Cint 3)
    let myConst :: T.Text
        myConst = "myConst"
    let fid = createFunctionId myConst 987654 [] sortIdInt
    let mapI = EnvNames (Map.fromList [(sortIdInt, "Sort_Int")])
                        Map.empty
                        (Map.fromList [(fid,myConst)])
    let (TXS2SMTFuncTest envdefs e) = createFunctionDef mapI fid [] sortIdInt ve
    assertEqual "Constant Function" e (T.unpack (funcdefsToSMT mapI (funcDefs envdefs)))

testSingleArg :: Test
testSingleArg = TestCase $ do
    let v = VarId "x" 645421 sortIdInt
    let ve = createVvar v
    let fName :: T.Text
        fName = "singleArgFunction"
    let fid = createFunctionId fName 987654 [v] sortIdInt
    let mapI = EnvNames (Map.fromList [(sortIdInt, "Sort_Int")])
                        Map.empty
                        (Map.fromList [(fid, fName)])
    let (TXS2SMTFuncTest envdefs e) = createFunctionDef mapI fid [v] sortIdInt ve
    assertEqual "single Argument Function" e (T.unpack (funcdefsToSMT mapI (funcDefs envdefs)))

testMultipleArgs :: Test
testMultipleArgs = TestCase $ do
    let varX = VarId "x" 645421 sortIdBool
    let varY = VarId "y" 645422 sortIdBool
    let ve = createVequal (createVvar varX) (createVvar varY)
    let fName :: T.Text
        fName = "multipleArgsFunction"
    let fid = createFunctionId fName 987654 [varX, varY] sortIdBool
    let mapI = EnvNames (Map.fromList [(sortIdBool, "SortBoolean")])
                        Map.empty
                        (Map.fromList [(fid, fName)])
    let (TXS2SMTFuncTest envdefs e) = createFunctionDef mapI fid [varX, varY] sortIdBool ve
    assertEqual "multiple Arguments Function" e (T.unpack (funcdefsToSMT mapI (funcDefs envdefs)))

testMultipleFunctions :: Test
testMultipleFunctions = TestCase $ do
    let fName1 :: T.Text
        fName1 = "multipleArgsFunction"
    let varX = VarId "x" 645421 sortIdBool
    let varY = VarId "y" 645422 sortIdBool
    let fid1 = createFunctionId fName1 987654 [varX, varY] sortIdBool
    let vexpr1 = createVequal (createVvar varX) (createVvar varY)

    let fName2 :: T.Text
        fName2 = "myConst"
    let fid2 = createFunctionId fName2 97531 [] sortIdInt
    let vexpr2 = createVconst (Cint 3)

    let mapI = EnvNames (Map.fromList [(sortIdBool, "boolean")
                                      ,(sortIdInt, "integer")
                                      ])
                        Map.empty
                        (Map.fromList [(fid1, fName1)
                                      ,(fid2, fName2)
                                      ])

    let (TXS2SMTFuncTest envdefs expected') = createFunctionDefsRecursive mapI [(fid1,[varX, varY],sortIdBool,vexpr1),(fid2,[],sortIdInt,vexpr2)]

    assertEqual "multiple Functions" expected' (T.unpack (funcdefsToSMT mapI (funcDefs envdefs)))
