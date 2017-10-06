{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where
import System.Exit
import Test.HUnit

import TestExternal
import TestFuncDefToSMT
import TestRecursiveFunction
import TestSMTCount
import TestSMTValue
import TestSortDefToSMT
import TestVexprToSMT


testSmtList :: Test
testSmtList = TestList [
        TestLabel "External"                testExternalList,
        TestLabel "FuncDefToSmt"            testFuncDefToSMTList,
        TestLabel "RecursiveFunction"       testRecursiveFunctionList,
        TestLabel "SMT Count"               testSMTCountList,
        TestLabel "SMTValue"                testSMTValueList,
        TestLabel "SortDefToSmt"            testSortDefToSMTList,
        TestLabel "VexprToSmt"              testVexprToSMTList
    ]

        
main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testSmtList
    if 0 == e+f
        then exitSuccess
        else exitFailure