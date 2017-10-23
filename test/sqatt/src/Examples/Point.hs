{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.Point (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "Point"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Stepper Test"
  , txsModelFiles = [txsFilePath exampDir "Point"]
  , txsCmdsFiles = [txsCmdPath exampDir "Point_Stepper"]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "First Quadrant Test Purpose"
  , txsModelFiles = [ txsFilePath exampDir "Point"
                    , txsFilePath exampDir "InFirstQuadrantTestPurpose" ]
  , txsCmdsFiles = [txsCmdPath exampDir "Point_FirstQuadrant_Tester"]
  , txsServerArgs = []
  , sutExample = Just (TxsSimulator $ txsCmdPath exampDir "Point_Simulator")
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Point" examples
