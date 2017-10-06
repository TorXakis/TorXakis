{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.Queue (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "Queue"

queueSUTPath :: FilePath
queueSUTPath = javaFilePath exampDir "QueueServer"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Stepper Test"
  , txsModelFiles = [txsFilePath exampDir "Queue"]
  , txsCmdsFiles = [txsCmdPath exampDir "Queue_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Stepper Test (Lossy)"
  , txsModelFiles = [txsFilePath exampDir "Queue"]
  , txsCmdsFiles = [txsCmdPath exampDir "Queue_Lossy_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test2 :: TxsExample
test2 = TxsExample
  { exampleName = "SUT Test"
  , txsModelFiles = [txsFilePath exampDir "Queue"]
  , txsCmdsFiles = [txsCmdPath exampDir "Queue_Tester"]
  , txsServerArgs = []
  , sutExample = Just (JavaExample queueSUTPath ["7890"])
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1, test2]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Queue" examples
