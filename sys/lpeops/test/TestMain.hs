{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import System.Exit
import Test.HUnit

import TestClean
import TestConstElm
import TestParElm
import TestDataReset
import TestParReset
import TestConfCheck
--import TestUGuard

testList :: Test
testList = TestList
    [
      TestLabel "testCleanEquivalence"     testCleanEquivalence
    , TestLabel "testCleanContainment"     testCleanContainment
    , TestLabel "cleanUnreachable"         testCleanUnreachable
    , TestLabel "constElmBasic"            testConstElmBasic
    , TestLabel "constElmXYX"              testConstElmXYX
    , TestLabel "parElmBasic"              testParElmBasic
    , TestLabel "parElmXUpperBound"        testParElmXUpperBound
    , TestLabel "dataReset"                testDataResetBasic
    , TestLabel "parResetBasic"            testParResetBasic
    , TestLabel "confCheckBasic"           testConfCheckBasic
    , TestLabel "confElmNoChange"          testConfElmNoChange
    , TestLabel "confElmBasic"             testConfElmBasic
    , TestLabel "confElmModulo"            testConfElmModulo
    -- , TestLabel "uguardBasic"              testUGuardBasic
    ]

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testList
    if 0 == e+f
    then exitSuccess
    else exitFailure
-- main

