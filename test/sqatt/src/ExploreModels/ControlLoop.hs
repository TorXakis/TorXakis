{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module ExploreModels.ControlLoop (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "ControlLoop"

multipleControlLoopTxsPath :: FilePath
multipleControlLoopTxsPath = txsFilePath exampDir "MultipleControlLoops"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Stepper 500"
  , txsModelFiles = [txsFilePath exampDir "ControlLoopModel"]
  , txsCmdsFiles = [txsCmdPath exampDir "ControlLoop_Stepper_Model"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Spec Produce 100"
  , txsModelFiles = [multipleControlLoopTxsPath]
  , txsCmdsFiles = [txsCmdPath exampDir "MultipleControlLoops_SpecProduce_Stepper_Model"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test2 :: TxsExample
test2 = TxsExample
  { exampleName = "Spec Measure 100"
  , txsModelFiles = [multipleControlLoopTxsPath]
  , txsCmdsFiles = [txsCmdPath exampDir "MultipleControlLoops_SpecMeasure_Stepper_Model"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test3 :: TxsExample
test3 = TxsExample
  { exampleName = "Spec Correct 100"
  , txsModelFiles = [multipleControlLoopTxsPath]
  , txsCmdsFiles = [txsCmdPath exampDir "MultipleControlLoops_SpecCorrect_Stepper_Model"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test4 :: TxsExample
test4 = TxsExample
  { exampleName = "Multiple Loops Stepper 30"
  , txsModelFiles = [multipleControlLoopTxsPath]
  , txsCmdsFiles = [txsCmdPath exampDir "MultipleControlLoops_Spec_Stepper_Model"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }


examples :: [TxsExample]
examples = [test0, test1, test2, test3, test4]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Control Loop #model" examples
