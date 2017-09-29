{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where
import System.Exit
import Test.HUnit

import TestXsd2Posix
import TestXsd2Smt

testSmtList :: Test
testSmtList = TestList [ TestLabel "Regex Xsd 2 Posix" testXsd2PosixList
                       , TestLabel "Regex Xsd 2 Smt"   testXsd2SmtList
                       ]

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testSmtList
    if 0 == e+f
        then exitSuccess
        else exitFailure