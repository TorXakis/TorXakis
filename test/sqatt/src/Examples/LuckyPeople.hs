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
  { exampleName = "Examples Purpose Test"
  , txsModelFile = txsFilePath specDir luckyPeopleText
  , txsCommandsFile = txsCmdPath exampDir "LuckyPeopleExamples_Purpose"
  , sutExample =
    Just (JavaExample
           (javaFilePath sutDir luckyPeopleText)
           []
         )
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Lucky By Gender Test"
  , txsModelFile = txsFilePath specDir luckyPeopleText
  , txsCommandsFile = txsCmdPath exampDir "LuckyPeopleByGender_Purpose"
  , sutExample =
    Just (JavaExample
           (javaFilePath sutDir luckyPeopleText)
           []
         )
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Lucky People" examples

