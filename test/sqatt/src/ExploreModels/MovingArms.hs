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
  { exampleName = "Stepper 200"
  , txsModelFiles = [txsFilePath exampDir "MovingArms"]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_Stepper_Model"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [testModel]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Moving Arms #model" examples
