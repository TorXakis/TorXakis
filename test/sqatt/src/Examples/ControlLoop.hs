{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.ControlLoop (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "ControlLoop"

multipleControlLoopTxsPath :: FilePath
multipleControlLoopTxsPath = txsFilePath exampDir "MultipleControlLoops"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Stepper"
  , txsModelFiles = [txsFilePath exampDir "ControlLoopModel"]
  , txsCmdsFiles = [txsCmdPath exampDir "ControlLoop_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Spec Produce"
  , txsModelFiles = [multipleControlLoopTxsPath]
  , txsCommandsFile = txsCmdPath exampDir "MultipleControlLoops_SpecProduce_Stepper"
  , sutExample = Nothing
  , expectedResult = Pass
  }

test2 :: TxsExample
test2 = TxsExample
  { exampleName = "Spec Measure"
  , txsModelFiles = [multipleControlLoopTxsPath]
  , txsCmdsFiles = [txsCmdPath exampDir "MultipleControlLoops_SpecMeasure_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test3 :: TxsExample
test3 = TxsExample
  { exampleName = "Spec Correct"
  , txsModelFiles = [multipleControlLoopTxsPath]
  , txsCmdsFiles = [txsCmdPath exampDir "MultipleControlLoops_SpecCorrect_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test4 :: TxsExample
test4 = TxsExample
  { exampleName = "Multiple Loops Stepper #long"
  , txsModelFiles = [multipleControlLoopTxsPath]
  , txsCmdsFiles = [txsCmdPath exampDir "MultipleControlLoops_Spec_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }


examples :: [TxsExample]
examples = [test0, test1, test2, test3, test4]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Control Loop" examples
