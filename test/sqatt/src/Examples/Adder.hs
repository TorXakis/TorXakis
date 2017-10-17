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
  , txsCmdsFiles = [txsCmdPath exampDir "Adder_Tester"] -- saves trace to AdderPurpose.txs for testReplayTrace
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
  { exampleName = "State Automation"
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

testReplayTrace :: TxsExample
testReplayTrace = TxsExample
  { exampleName = "Replay Adder Trace"
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

testPurp1 :: TxsExample
testPurp1 = TxsExample
  { exampleName = "Purp1 - 4 goals"
  , txsModelFiles = [ txsFilePath exampDir "Adder"
                    , txsFilePath exampDir "AdderPurposes"
                    ]
  , txsCmdsFiles = [txsCmdPath exampDir "AdderPurp1"]
  , txsServerArgs = []
  , sutExample =
    Just (JavaExample
           adderJavaPath
           ["7890"]
         )
  , expectedResult = Pass
  }

testPurp2 :: TxsExample
testPurp2 = TxsExample
  { exampleName = "Purp2 - Operand constraints"
  , txsModelFiles = [ txsFilePath exampDir "Adder"
                    , txsFilePath exampDir "AdderPurposes"
                    ]
  , txsCmdsFiles = [txsCmdPath exampDir "AdderPurp2"]
  , txsServerArgs = []
  , sutExample =
    Just (JavaExample
           adderJavaPath
           ["7890"]
         )
  , expectedResult = Pass
  }

testPurp3 :: TxsExample
testPurp3 = TxsExample
  { exampleName = "Purp3 - Always +2"
  , txsModelFiles = [ txsFilePath exampDir "Adder"
                    , txsFilePath exampDir "AdderPurposes"
                    ]
  , txsCmdsFiles = [txsCmdPath exampDir "AdderPurp3"]
  , txsServerArgs = []
  , sutExample =
    Just (JavaExample
           adderJavaPath
           ["7890"]
         )
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1, test2, testReplayTrace, testPurp1, testPurp2, testPurp3]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Adder" examples
