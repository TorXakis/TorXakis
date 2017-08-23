{-# LANGUAGE OverloadedStrings #-}
module Examples.Adder (exampleSet) where

import           Examples.Paths
import           Filesystem.Path
import           Prelude         hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "Adder"

adderJavaPath :: FilePath
adderJavaPath = javaFilePath exampDir "Adder"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Basic"
  , txsModelFile = txsFilePath exampDir "Adder"
  , txsCommandsFile = txsCmdPath exampDir "Adder_Tester"
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
  , txsModelFile = txsFilePath exampDir "AdderStAut"
  , txsCommandsFile = txsCmdPath exampDir "Adder_Tester"
  , sutExample =
    Just (JavaExample
           adderJavaPath
           ["7890"]
         )
  , expectedResult = Pass
  }

test2 :: TxsExample
test2 = TxsExample
  { exampleName = "Parallel adders"
  , txsModelFile = txsFilePath exampDir "Adder"
  , txsCommandsFile = txsCmdPath exampDir "Adder3_Tester"
  , sutExample =
    Just (JavaExample
           adderJavaPath
           ["7891", "7892", "7893"]
         )
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test0, test1, test2]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Adder" examples
