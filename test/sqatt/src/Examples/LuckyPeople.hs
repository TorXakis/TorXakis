{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.LuckyPeople (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

luckyPeopleText :: Text
luckyPeopleText = "LuckyPeople"

exampDir :: FilePath
exampDir = "LuckyPeople"

specDir :: FilePath
specDir = exampDir </> "spec"

sutDir :: FilePath
sutDir = exampDir </> "sut"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Test 0"
  , txsModelFile = txsFilePath specDir luckyPeopleText
  , txsCommandsFile = txsCmdPath exampDir "LuckyPeopleExamples_Tester"
  , sutExample =
    Just (JavaExample
           (javaFilePath sutDir luckyPeopleText)
           []
         )
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Test 1"
  , txsModelFile = txsFilePath specDir luckyPeopleText
  , txsCommandsFile = txsCmdPath exampDir "LuckyPeopleRandom_Tester"
  , sutExample =
    Just (JavaExample
           (javaFilePath sutDir luckyPeopleText)
           []
         )
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test1, test0]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Lucky People" examples

