{-# LANGUAGE OverloadedStrings #-}
module Examples.StimulusResponse (exampleSet) where

import           Data.Text
import           Examples.Paths
import           Filesystem.Path
import           Prelude         hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "stimulusresponse"

stimulusResponseName :: Text
stimulusResponseName = "StimulusResponse"

stimulusNoResponseName :: Text
stimulusNoResponseName = "StimulusNoResponse"

stimulusResponseLoopName :: Text
stimulusResponseLoopName = "StimulusResponseLoop"

stimulusResponseTxsPath :: FilePath
stimulusResponseTxsPath = txsFilePath exampDir stimulusResponseName

stimulusNoResponseTxsPath :: FilePath
stimulusNoResponseTxsPath = txsFilePath exampDir stimulusNoResponseName

stimulusResponseLoopTxsPath :: FilePath
stimulusResponseLoopTxsPath = txsFilePath exampDir stimulusResponseLoopName

stimulusResponseTxsCmdPath :: FilePath
stimulusResponseTxsCmdPath = txsCmdPath exampDir stimulusResponseName

stimulusResponseJavaPath :: FilePath
stimulusResponseJavaPath = javaFilePath exampDir stimulusResponseName

stimulusNoResponseJavaPath :: FilePath
stimulusNoResponseJavaPath = javaFilePath exampDir stimulusNoResponseName

stimulusResponseLoopJavaPath :: FilePath
stimulusResponseLoopJavaPath = javaFilePath exampDir stimulusResponseLoopName

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Stimulus-Response Test 0"
  , txsModelFile = stimulusResponseTxsPath
  , txsCommandsFile = stimulusResponseTxsCmdPath
  , sutSourceFile = Just stimulusResponseJavaPath
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Stimulus-Response Test 1"
  , txsModelFile = stimulusResponseTxsPath
  , txsCommandsFile = stimulusResponseTxsCmdPath
  , sutSourceFile = Just stimulusNoResponseJavaPath
  , expectedResult = Fail
  }

test2 :: TxsExample
test2 = TxsExample
  { exampleName = "Stimulus-Response Test 2"
  , txsModelFile = stimulusResponseLoopTxsPath
  , txsCommandsFile = stimulusResponseTxsCmdPath
  , sutSourceFile = Just stimulusResponseLoopJavaPath
  , expectedResult = Pass
  }

test3 :: TxsExample
test3 = TxsExample
  { exampleName = "Stimulus-Response Test 3"
  , txsModelFile = stimulusResponseLoopTxsPath
  , txsCommandsFile = stimulusResponseTxsCmdPath
  , sutSourceFile = Just stimulusResponseJavaPath
  , expectedResult = Fail
  }

test4 :: TxsExample
test4 = TxsExample
  { exampleName = "Stimulus-Response Test 4"
  , txsModelFile = stimulusResponseTxsPath
  , txsCommandsFile = stimulusResponseTxsCmdPath
  , sutSourceFile = Just stimulusResponseLoopJavaPath
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1, test2, test3, test4]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Stimulus-Response" examples
