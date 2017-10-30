{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module ExploreModels.ReadWriteConflict (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

readWriteConflictText :: Text
readWriteConflictText = "ReadWrite"

readWriteConflictAdvancedText :: Text
readWriteConflictAdvancedText = "ReadWriteAdvanced"

exampDir :: FilePath
exampDir = "ReadWriteConflict"

testStepper :: TxsExample
testStepper = TxsExample
  { exampleName = "Stepper 100"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_Stepper_Model"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

testAdvancedStepper :: TxsExample
testAdvancedStepper = TxsExample
  { exampleName = "Advanced Stepper 100"
  , txsModelFiles = [txsFilePath exampDir readWriteConflictAdvancedText]
  , txsCmdsFiles = [txsCmdPath exampDir "ReadWrite_Stepper_Model"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [testStepper,testAdvancedStepper]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "ReadWrite Conflict #model" examples
