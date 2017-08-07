{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where
import Test.HUnit
import System.Exit

import TestBehave
import TestLib

tests = TestList    [ TestLabel "Library Tests"        testLibList
                    , TestLabel "Behave Tests"         testBehaveList
                    ]                  

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT tests
    if e+f == 0
        then exitSuccess
        else exitFailure