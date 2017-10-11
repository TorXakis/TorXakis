{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.MovingArms (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "MovingArms"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Stepper Test"
  , txsModelFiles = [txsFilePath exampDir "MovingArms"]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

testSingleAxisPurpose :: TxsExample
testSingleAxisPurpose = TxsExample
  { exampleName = "Single Axis Purpose Test"
  , txsModelFiles = [ txsFilePath exampDir "MovingArms"
                    , txsFilePath exampDir "SingleAxisPurpose" ]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_SingleAxisPurpose_Tester"]
  , txsServerArgs = []
  , sutExample = Just (TxsSimulator $ txsCmdPath exampDir "MovingArms_Simulator")
  , expectedResult = Pass
  }

testRestrictedAxisPurpose :: TxsExample
testRestrictedAxisPurpose = TxsExample
  { exampleName = "Restricted Axis Purpose Test"
  , txsModelFiles = [ txsFilePath exampDir "MovingArms"
                    , txsFilePath exampDir "RestrictedAxisPurpose" ]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_RestrictedAxisPurpose_Tester"]
  , txsServerArgs = []
  , sutExample = Just (TxsSimulator $ txsCmdPath exampDir "MovingArms_Simulator")
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, testSingleAxisPurpose, testRestrictedAxisPurpose]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Moving Arms" examples
