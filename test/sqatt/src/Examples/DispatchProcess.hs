{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.DispatchProcess (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "DispatchProcess"

specDir :: FilePath
specDir = exampDir </> "spec"

sutDir :: FilePath
sutDir = exampDir </> "sut"

dispatchProcessCmdsPath :: FilePath
dispatchProcessCmdsPath = txsCmdPath exampDir "DispatchProcess_Stepper"

dispatchProcessSUTPath :: FilePath
dispatchProcessSUTPath = javaFilePath sutDir "DispatchProcess"

javaSUT :: Maybe SutExample
javaSUT = Just (JavaExample dispatchProcessSUTPath [])

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Process 01"
  , txsModelFiles = [txsFilePath specDir "DisPro01-processor"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Process 02"
  , txsModelFiles = [txsFilePath specDir "DisPro02-dispatch"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test2 :: TxsExample
test2 = TxsExample
  { exampleName = "Process 03"
  , txsModelFiles = [txsFilePath specDir "DisPro03-processors"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test3 :: TxsExample
test3 = TxsExample
  { exampleName = "Process 04"
  , txsModelFiles = [txsFilePath specDir "DisPro04-hide"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test4 :: TxsExample
test4 = TxsExample
  { exampleName = "Process 05"
  , txsModelFiles = [txsFilePath specDir "DisPro05-data"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }


test5 :: TxsExample
test5 = TxsExample
  { exampleName = "Process 05a"
  , txsModelFiles = [txsFilePath specDir "DisPro05a-data-nohide"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test6 :: TxsExample
test6 = TxsExample
  { exampleName = "Process 06"
  , txsModelFiles = [txsFilePath specDir "DisPro06-datapos"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test7 :: TxsExample
test7 = TxsExample
  { exampleName = "Process 06a"
  , txsModelFiles = [txsFilePath specDir "DisPro06a-datapos-nohide"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test8 :: TxsExample
test8 = TxsExample
  { exampleName = "Process 07"
  , txsModelFiles = [txsFilePath specDir "DisPro07-gcd"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test9 :: TxsExample
test9 = TxsExample
  { exampleName = "Process 08"
  , txsModelFiles = [txsFilePath specDir "DisPro08-gcdpurp"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test10 :: TxsExample
test10 = TxsExample
  { exampleName = "Process 09"
  , txsModelFiles = [txsFilePath specDir "DisPro09-multiproc"]
  , txsCommandsFile = dispatchProcessCmdsPath
  , sutExample = Nothing
  , expectedResult = Pass
  }

test11 :: TxsExample
test11 = TxsExample
  { exampleName = "Process 10 - Data"
  , txsModelFiles = [txsFilePath specDir "DisPro10-data"]
  , txsCommandsFile = txsCmdPath exampDir "DispatchProcess_Tester"
  , sutExample = javaSUT
  , expectedResult = Pass
  }

-- Disabled for now: See https://github.com/TorXakis/TorXakis/issues/100
-- test12 :: TxsExample
-- test12 = TxsExample
--   { exampleName = "Process 12 - Unique ID"
--   , txsModelFiles = [txsFilePath specDir "DisPro12-unique-id"]
--   , txsCommandsFile = txsCmdPath exampDir "DispatchProcess_Tester_12"
--   , sutExample = javaSUT
--   , expectedResult = Pass
--   }

test13 :: TxsExample
test13 = TxsExample
  { exampleName = "Process 12 - Unique ID (Wrong)"
  , txsModelFiles = [txsFilePath specDir "DisPro12-unique-id"]
  , txsCommandsFile = txsCmdPath exampDir "DispatchProcess_Tester_12_Wrong"
  , sutExample = javaSUT
  , expectedResult = Fail
  }

test14 :: TxsExample
test14 = TxsExample
  { exampleName = "Process 12 - Unique ID (Right)"
  , txsModelFiles = [txsFilePath specDir "DisPro12-unique-id"]
  , txsCommandsFile = txsCmdPath exampDir "DispatchProcess_Tester_12_Right"
  , sutExample = javaSUT
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [ test0, test1, test2, test3, test4, test5, test6, test7, test8
           , test9, test10, test11, test13, test14
           ]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Dispatch Process" examples
