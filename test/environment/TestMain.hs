{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where
import Test.HUnit
import System.Exit

import TestPreludeRead
import TestTextXMLExpatTree

testList :: Test
testList = TestList [
        TestLabel "Prelude Read"                        testPreludeReadList
      , TestLabel "Text.XML.Expat.Tree"                 testTextXMLExpatTreeList
    ]
    

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testList
    if e+f == 0
        then exitSuccess
        else exitFailure