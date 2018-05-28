{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where

import System.Exit

import TestBehExprDefs

testDefsPackage :: IO Bool
testDefsPackage = testBehExprDefs   

main :: IO ()
main = do
    result <- testDefsPackage
    if result
        then exitSuccess
        else exitFailure