{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.StimulusResponse (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "StimulusResponse"

stimulusResponseName :: Text
stimulusResponseName = "StimulusResponse"

stimulusNoResponseName :: Text
stimulusNoResponseName = "StimulusNoResponse"

stimulusResponseLoopName :: Text
stimulusResponseLoopName = "StimulusResponseLoop"

stimulusResponseTxsPath :: FilePath
stimulusResponseTxsPath = txsFilePath exampDir stimulusResponseName

stimulusResponseLoopTxsPath :: FilePath
stimulusResponseLoopTxsPath = txsFilePath exampDir stimulusResponseLoopName

stimulusResponseTxsCmdPath :: FilePath
stimulusResponseTxsCmdPath = txsCmdPath exampDir "StimulusResponse_Tester"

stimulusResponseJavaPath :: FilePath
stimulusResponseJavaPath = javaFilePath exampDir stimulusResponseName

stimulusNoResponseJavaPath :: FilePath
stimulusNoResponseJavaPath = javaFilePath exampDir stimulusNoResponseName

stimulusResponseLoopJavaPath :: FilePath
stimulusResponseLoopJavaPath = javaFilePath exampDir stimulusResponseLoopName

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Stimulus-Response Test 0"
  , txsModelFiles = [stimulusResponseTxsPath]
  , txsCmdsFiles = [stimulusResponseTxsCmdPath]
  , txsServerArgs = []
  , sutExample = Just (JavaExample stimulusResponseJavaPath [])
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Stimulus-Response Test 1"
  , txsModelFiles = [stimulusResponseTxsPath]
  , txsCmdsFiles = [stimulusResponseTxsCmdPath]
  , txsServerArgs = []
  , sutExample = Just (JavaExample stimulusNoResponseJavaPath [])
  , expectedResult = Fail
  }

test2 :: TxsExample
test2 = TxsExample
  { exampleName = "Stimulus-Response Test 2"
  , txsModelFiles = [stimulusResponseLoopTxsPath]
  , txsCmdsFiles = [stimulusResponseTxsCmdPath]
  , txsServerArgs = []
  , sutExample = Just (JavaExample stimulusResponseLoopJavaPath [])
  , expectedResult = Pass
  }

test3 :: TxsExample
test3 = TxsExample
  { exampleName = "Stimulus-Response Test 3"
  , txsModelFiles = [stimulusResponseLoopTxsPath]
  , txsCmdsFiles = [stimulusResponseTxsCmdPath]
  , txsServerArgs = []
  , sutExample = Just (JavaExample stimulusResponseJavaPath [])
  , expectedResult = Fail
  }

test4 :: TxsExample
test4 = TxsExample
  { exampleName = "Stimulus-Response Test 4"
  , txsModelFiles = [stimulusResponseTxsPath]
  , txsCmdsFiles = [stimulusResponseTxsCmdPath]
  , txsServerArgs = []
  , sutExample = Just (JavaExample stimulusResponseLoopJavaPath [])
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1, test2, test3, test4]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Stimulus-Response" examples
