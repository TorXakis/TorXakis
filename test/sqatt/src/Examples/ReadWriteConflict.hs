{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.ReadWriteConflict (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

readWriteConflictText :: Text
readWriteConflictText = "ReadWrite"

readWriteConflictTestPurpText :: Text
readWriteConflictTestPurpText = "ReadWriteTestPurposes"

readWriteConflictAdvancedText :: Text
readWriteConflictAdvancedText = "ReadWriteAdvanced"

exampDir :: FilePath
exampDir = "ReadWriteConflict"

testStepper :: TxsExample
testStepper = emptyExample
  { exampleName = "Stepper Test"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_Stepper"]
  , expectedResult = Pass
  }

testPurpStepper :: TxsExample
testPurpStepper = emptyExample
  { exampleName = "Stepper Test for Purp Model"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictTestPurpText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_Stepper"]
  , expectedResult = Pass
  }
  
testHitAll7 :: TxsExample
testHitAll7 = emptyExample
  { exampleName = "HitAll7 Purpose Test"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictTestPurpText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_HitAll7Purpose_Tester"]
  , sutExample = Just (TxsSimulator $ txsCmdPath exampDir "ReadWrite_Simulator")
  , expectedResult = Pass
  }

testAdvancedStepper :: TxsExample
testAdvancedStepper = emptyExample
  { exampleName = "Advanced Stepper Test"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictAdvancedText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_Stepper"]
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [testStepper, testPurpStepper,testHitAll7,testAdvancedStepper]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "ReadWrite Conflict" examples
