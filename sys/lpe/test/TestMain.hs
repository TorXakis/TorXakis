{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where
import System.Exit
import Test.HUnit

import TestPreGNF
import TestGNF
import TestLPEHelpers

testLPEList :: Test
testLPEList = TestList
    [   TestLabel "preGNF"          testPreGNFList
       , TestLabel "GNF"             testGNFList
      , TestLabel "LPEHelpers"      testLPEHelpersList
    ]

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testLPEList
    if 0 == e+f
        then exitSuccess
        else exitFailure
