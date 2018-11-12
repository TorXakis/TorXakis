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
import TestLPE
import TestLPEPar
import TestLPEHide
import TestPreGNFEnable
import TestPreGNFDisable
import TestLPEInterrupt

testList :: Test
testList = TestList
    [
          TestLabel "preGNF"          testPreGNFList
        , TestLabel "GNF"             testGNFList
        , TestLabel "LPE"             testLPEList
        , TestLabel "LPEPar"          testLPEParList
        , TestLabel "LPEHide"         testLPEHideList
        , TestLabel "PreGNFEnable"    testPreGNFEnableList
        , TestLabel "PreGNFDisable"   testPreGNFDisableList
        , TestLabel "LPEInterrupt"    testLPEInterruptList
    ]

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testList
    if 0 == e+f
        then exitSuccess
        else exitFailure
