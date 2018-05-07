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
test0 = emptyExample
  { exampleName = "Stepper Test"
  , txsModelFiles = [txsFilePath exampDir "MovingArms"]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_Stepper"]
  , expectedResult = Pass
  }

testMovingArm :: TxsExample
testMovingArm = emptyExample
  { exampleName = "Moving Arm Test"
  , txsModelFiles = [ txsFilePath exampDir "MovingArm" ]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArm_Tester"]
  , sutExample = Just (TxsSimulator $ txsCmdPath exampDir "MovingArm_Simulator")
  , expectedResult = Pass
  }

testSingleAxisPurpose :: TxsExample
testSingleAxisPurpose = emptyExample
  { exampleName = "Single Axis Purpose Test"
  , txsModelFiles = [ txsFilePath exampDir "MovingArms"
                    , txsFilePath exampDir "SingleAxisPurpose" ]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_SingleAxisPurpose_Tester"]
  , sutExample = Just (TxsSimulator $ txsCmdPath exampDir "MovingArms_Simulator")
  , expectedResult = Pass
  }

testRestrictedAxisPurpose0 :: TxsExample
testRestrictedAxisPurpose0 = emptyExample
  { exampleName = "Restricted Axis Purpose Test (input eagerness 0)"
  , txsModelFiles = [ txsFilePath exampDir "MovingArms"
                    , txsFilePath exampDir "RestrictedAxisPurpose" ]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_RestrictedAxisPurpose_eager0_Tester"]
  , sutExample = Just (TxsSimulator $ txsCmdPath exampDir "MovingArms_Simulator")
  , expectedResult = Pass
  }

testRestrictedAxisPurpose3 :: TxsExample
testRestrictedAxisPurpose3 = emptyExample
  { exampleName = "Restricted Axis Purpose Test (input eagerness 3)"
  , txsModelFiles = [ txsFilePath exampDir "MovingArms"
                    , txsFilePath exampDir "RestrictedAxisPurpose" ]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_RestrictedAxisPurpose_eager3_Tester"]
  , sutExample = Just (TxsSimulator $ txsCmdPath exampDir "MovingArms_Simulator")
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, testMovingArm, testSingleAxisPurpose, testRestrictedAxisPurpose0, testRestrictedAxisPurpose3]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Moving Arms" examples
