{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.Adder (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "Adder"

adderJavaPath :: FilePath
adderJavaPath = javaFilePath exampDir "Adder"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Basic"
  , txsModelFiles = [txsFilePath exampDir "Adder"]
  , txsCmdsFiles = [txsCmdPath exampDir "Adder_Tester"]
  , txsServerArgs = []
  , sutExample =
    Just (JavaExample
           adderJavaPath
           ["7890"]
         )
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "State Automaton"
  , txsModelFiles = [txsFilePath exampDir "AdderStAut"]
  , txsCmdsFiles = [txsCmdPath exampDir "AdderStautTester"]
  , txsServerArgs = []
  , sutExample =
    Just (JavaExample
           adderJavaPath
           ["7890"]
         )
  , expectedResult = Pass
  }

test2 :: TxsExample
test2 = TxsExample
  { exampleName = "Parallel Adders"
  , txsModelFiles = [txsFilePath exampDir "Adder"]
  , txsCmdsFiles = [txsCmdPath exampDir "Adder3_Tester"]
  , txsServerArgs = []
  , sutExample =
    Just (JavaExample
           adderJavaPath
           ["7891", "7892", "7893"]
         )
  , expectedResult = Pass
  }


test3 :: TxsExample
test3 = TxsExample
  { exampleName = "Replay Adder"
  , txsModelFiles = [ txsFilePath exampDir "Adder"
                    , txsFilePath exampDir "AdderReplay"
                    , txsPurposeFromTracePath "AdderPurpose"
                    ]
  , txsCmdsFiles = [txsCmdPath exampDir "AdderReplay"]
  , txsServerArgs = []
  , sutExample =
    Just (JavaExample
           adderJavaPath
           ["7890"]
         )
  , expectedResult = Message "Goal replayAdd: Hit"
  }

examples :: [TxsExample]
examples = [test0, test1, test2, test3]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Adder" examples
