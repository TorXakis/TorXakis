{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module ExploreModels.MovingArms (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "MovingArms"

testModel :: TxsExample
testModel = TxsExample
  { exampleName = "Stepper 500"
  , txsModelFiles = [txsFilePath exampDir "MovingArms"]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_Stepper_Model"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

testRestrictedAxisPurpose0 :: TxsExample
testRestrictedAxisPurpose0 = TxsExample
  { exampleName = "Restricted Axis Purpose Test (input eagerness 0) 100"
  , txsModelFiles = [ txsFilePath exampDir "MovingArms"
                    , txsFilePath exampDir "RestrictedAxisPurpose" ]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_RestrictedAxisPurpose_eager0_Tester_Model"]
  , txsServerArgs = []
  , sutExample = Just (TxsSimulator $ txsCmdPath exampDir "MovingArms_Simulator_Model")
  , expectedResult = Pass
  }
  
examples :: [TxsExample]
examples = [testModel, testRestrictedAxisPurpose0]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Moving Arms #model" examples
