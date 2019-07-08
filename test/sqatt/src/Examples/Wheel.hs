{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.Wheel (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "Wheel"

wheelName :: Text
wheelName = "Wheel"

wheelTxsPath :: FilePath
wheelTxsPath = txsFilePath exampDir wheelName

wheelStepperCmdPath :: FilePath
wheelStepperCmdPath = txsCmdPath exampDir "Wheel_Stepper"

test0 :: TxsExample
test0 = emptyExample
  { exampleName = "Stepper"
  , txsModelFiles = [wheelTxsPath]
  , txsCmdsFiles = [wheelStepperCmdPath]
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Wheel" examples
