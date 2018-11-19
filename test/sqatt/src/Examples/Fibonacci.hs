{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.Fibonacci (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "Fibonacci"

fiboTxsPath :: FilePath
fiboTxsPath = txsFilePath exampDir "Fibonacci"

fiboStdiTxsPath :: FilePath
fiboStdiTxsPath = txsFilePath exampDir "FibonacciStdi"

fiboStepperCmdPath :: FilePath
fiboStepperCmdPath = txsCmdPath exampDir "Fibonacci"


test0 :: TxsExample
test0 = emptyExample
  { exampleName = "Fibonacci"
  , txsModelFiles = [fiboTxsPath]
  , txsCmdsFiles = [fiboStepperCmdPath]
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = emptyExample
  { exampleName = "Fibonacci (stdi)"
  , txsModelFiles = [fiboStdiTxsPath]
  , txsCmdsFiles = [fiboStepperCmdPath]
  , expectedResult = Pass
  }


examples :: [TxsExample]
examples = [test0, test1]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Fibonacci" examples
