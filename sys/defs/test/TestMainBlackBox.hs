{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

module Main where

import System.Exit

import TestSum

test :: IO Bool
test = do
    resultSum <- testSum
    
    return resultSum   

main :: IO ()
main = do
    result <- test
    if result
        then exitSuccess
        else exitFailure