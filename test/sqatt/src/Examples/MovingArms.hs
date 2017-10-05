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

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Purpose Stepper Test #long"
  , txsModelFiles = [txsFilePath exampDir "MovingArms"]
  , txsCmdsFiles = [txsCmdPath exampDir "MovingArms_Purpose_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Moving Arms" examples
