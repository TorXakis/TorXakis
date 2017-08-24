{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.Point (exampleSet) where

import           Examples.Paths
import           Filesystem.Path
import           Prelude         hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "Point"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Stepper Test"
  , txsModelFile = txsFilePath exampDir "Point"
  , txsCommandsFile = txsCmdPath exampDir "Point_Stepper"
  , sutExample = Nothing
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Purpose Stepper Test"
  , txsModelFile = txsFilePath exampDir "Point"
  , txsCommandsFile = txsCmdPath exampDir "Point_Purpose_Stepper"
  , sutExample = Nothing
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Point" examples
