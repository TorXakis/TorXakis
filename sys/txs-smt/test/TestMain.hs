{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where
import System.Exit
import Test.HUnit

import TestConstraint
import TestPushPop

-- | list of tests
testSmtList :: Test
testSmtList = TestList 
    [ TestLabel "Constraint"              testConstraintList
    , TestLabel "Push Pop"                testPushPopList
    ]

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testSmtList
    if 0 == e+f
        then exitSuccess
        else exitFailure