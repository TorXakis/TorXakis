{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.Echo (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "Echo"

echoName :: Text
echoName = "Echo"

echoTxsPath :: FilePath
echoTxsPath = txsFilePath exampDir echoName

echoStepperCmdPath :: FilePath
echoStepperCmdPath = txsCmdPath exampDir "Echo_Stepper"

echoSimulatorCmdPath :: FilePath
echoSimulatorCmdPath = txsCmdPath exampDir "Echo_Simulator"

echoTesterCmdPath :: FilePath
echoTesterCmdPath = txsCmdPath exampDir "Echo_Tester"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Stepper"
  , txsModelFiles = [echoTxsPath]
  , txsCmdsFiles = [echoStepperCmdPath]
  , sutExample = Nothing
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Simulator"
  , txsModelFiles = [echoTxsPath]
  , txsCmdsFiles = [echoTesterCmdPath]
  , sutExample = Just (TxsSimulator echoSimulatorCmdPath)
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Echo" examples
