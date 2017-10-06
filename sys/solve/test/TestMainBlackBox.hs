{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where
import System.Exit
import Test.HUnit

import TestConstraint
import TestMultipleInstances
import TestPushPop
import TestXSD2SMT


testSmtList :: Test
testSmtList = TestList 
    [ TestLabel "Constraint"              testConstraintList
    , TestLabel "Multiple Instances"      testMultipleInstancesList
    , TestLabel "Push Pop"                testPushPopList
    , TestLabel "XSD 2 SMT"               testXSD2SMTList
    ]

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testSmtList
    if 0 == e+f
        then exitSuccess
        else exitFailure