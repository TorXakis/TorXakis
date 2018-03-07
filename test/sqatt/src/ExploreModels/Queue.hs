{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module ExploreModels.Queue (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "Queue"

testModel :: TxsExample
testModel = emptyExample
  { exampleName = "Stepper Test 200"
  , txsModelFiles = [txsFilePath exampDir "Queue"]
  , txsCmdsFiles = [txsCmdPath exampDir "Queue_Stepper_Model"]
  , expectedResult = Pass
  }

testLossyModel :: TxsExample
testLossyModel = emptyExample
  { exampleName = "Stepper Test (Lossy) 200"
  , txsModelFiles = [txsFilePath exampDir "Queue"]
  , txsCmdsFiles = [txsCmdPath exampDir "Queue_Lossy_Stepper_Model"]
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [testModel, testLossyModel]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Queue #model" examples
