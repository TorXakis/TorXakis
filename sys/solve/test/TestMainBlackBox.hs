{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

module Main where
import System.Exit
import Test.HUnit

import TestConstraint
import TestMultipleInstances
import TestPushPop


testSmtList :: Test
testSmtList = TestList [
        TestLabel "Constraint"              testConstraintList,
        TestLabel "Multiple Instances"      testMultipleInstancesList,
        TestLabel "Push Pop"                testPushPopList
    ]

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testSmtList
    if 0 == e+f
        then exitSuccess
        else exitFailure