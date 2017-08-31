{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where
import System.Exit
import Test.HUnit

import TestBExpr
import TestFuncContent
import TestProcessBehaviour

testBehaveList :: Test
testBehaveList = TestList
    [ TestLabel "Function Content"          testFuncContentList
    , TestLabel "ProcessDefinition Content" testBExprList
    , TestLabel "Process Behaviour"         testProcessBehaviourList
    ]

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testBehaveList
    if 0 == e+f
        then exitSuccess
        else exitFailure
