{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module ExploreModels.LuckyPeople (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

luckyPeopleText :: Text
luckyPeopleText = "LuckyPeople"

exampDir :: FilePath
exampDir = "LuckyPeople"

specDir :: FilePath
specDir = exampDir </> "spec"

testModel :: TxsExample
testModel = TxsExample
  { exampleName = "Stepper 200"
  , txsModelFiles = map (txsFilePath specDir)  [luckyPeopleText]
  , txsCmdsFiles = [txsCmdPath exampDir "LuckyPeople_Stepper_Model"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [testModel]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Lucky People #model" examples

