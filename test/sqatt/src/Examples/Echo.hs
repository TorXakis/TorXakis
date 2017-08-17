{-# LANGUAGE OverloadedStrings #-}
module Examples.Echo (exampleSet) where

import           Data.Text
import           Examples.Paths
import           Filesystem.Path
import           Prelude         hiding (FilePath)
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

test0 = TxsExample
  { exampleName = "Stepper"
  , txsModelFile = echoTxsPath
  , txsCommandsFile = echoStepperCmdPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test1 = TxsExample
  { exampleName = "Simulator"
  , txsModelFile = echoTxsPath
  , txsCommandsFile = echoTesterCmdPath
  , sutExample = Just (TxsSimulator echoSimulatorCmdPath)
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Echo" examples
