{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module ExploreModels.DispatchProcess (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

exampDir :: FilePath
exampDir = "DispatchProcess"

specDir :: FilePath
specDir = exampDir </> "spec"

dispatchProcessCmdsPath :: FilePath
dispatchProcessCmdsPath = txsCmdPath exampDir "DispatchProcess_Stepper_Model"

test0 :: TxsExample
test0 = TxsExample
  { exampleName = "Process 01"
  , txsModelFiles = [txsFilePath specDir "DisPro01-processor"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = TxsExample
  { exampleName = "Process 02"
  , txsModelFiles = [txsFilePath specDir "DisPro02-dispatch"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test2 :: TxsExample
test2 = TxsExample
  { exampleName = "Process 03"
  , txsModelFiles = [txsFilePath specDir "DisPro03-processors"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test3 :: TxsExample
test3 = TxsExample
  { exampleName = "Process 04"
  , txsModelFiles = [txsFilePath specDir "DisPro04-hide"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test4 :: TxsExample
test4 = TxsExample
  { exampleName = "Process 05"
  , txsModelFiles = [txsFilePath specDir "DisPro05-data"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }


test5 :: TxsExample
test5 = TxsExample
  { exampleName = "Process 05a"
  , txsModelFiles = [txsFilePath specDir "DisPro05a-data-nohide"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test6 :: TxsExample
test6 = TxsExample
  { exampleName = "Process 06"
  , txsModelFiles = [txsFilePath specDir "DisPro06-datapos"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test7 :: TxsExample
test7 = TxsExample
  { exampleName = "Process 06a"
  , txsModelFiles = [txsFilePath specDir "DisPro06a-datapos-nohide"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test8 :: TxsExample
test8 = TxsExample
  { exampleName = "Process 07"
  , txsModelFiles = [txsFilePath specDir "DisPro07-gcd"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test9 :: TxsExample
test9 = TxsExample
  { exampleName = "Process 08"
  , txsModelFiles = [txsFilePath specDir "DisPro08-gcdpurp"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test10 :: TxsExample
test10 = TxsExample
  { exampleName = "Process 09"
  , txsModelFiles = [txsFilePath specDir "DisPro09-multiproc"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

test11 :: TxsExample
test11 = TxsExample
  { exampleName = "Process 10 - Data"
  , txsModelFiles = [txsFilePath specDir "DisPro10-data"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , txsServerArgs = []
  , sutExample = Nothing
  , expectedResult = Pass
  }

-- test12 :: TxsExample
-- test12 = TxsExample
--   { exampleName = "Process 12 - Unique ID"
--   , txsModelFiles = [txsFilePath specDir "DisPro12-unique-id"]
--   , txsCmdsFiles = [txsCmdPath exampDir "DispatchProcess_Stepper_Model_12"]
--   , txsServerArgs = []
--   , sutExample = Nothing
--   , expectedResult = Pass
--   }

examples :: [TxsExample]
examples = [ test0, test1, test2, test3, test4, test5, test6, test7, test8
           , test9, test10, test11 -- , test12
           ]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Dispatch Process #model" examples
