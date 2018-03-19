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
test0 = emptyExample
  { exampleName = "Process 01"
  , txsModelFiles = [txsFilePath specDir "DisPro01-processor"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

test1 :: TxsExample
test1 = emptyExample
  { exampleName = "Process 02"
  , txsModelFiles = [txsFilePath specDir "DisPro02-dispatch"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

test2 :: TxsExample
test2 = emptyExample
  { exampleName = "Process 03"
  , txsModelFiles = [txsFilePath specDir "DisPro03-processors"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

test3 :: TxsExample
test3 = emptyExample
  { exampleName = "Process 04"
  , txsModelFiles = [txsFilePath specDir "DisPro04-hide"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

test4 :: TxsExample
test4 = emptyExample
  { exampleName = "Process 05"
  , txsModelFiles = [txsFilePath specDir "DisPro05-data"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }


test5 :: TxsExample
test5 = emptyExample
  { exampleName = "Process 05a"
  , txsModelFiles = [txsFilePath specDir "DisPro05a-data-nohide"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

test6 :: TxsExample
test6 = emptyExample
  { exampleName = "Process 06"
  , txsModelFiles = [txsFilePath specDir "DisPro06-datapos"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

test7 :: TxsExample
test7 = emptyExample
  { exampleName = "Process 06a"
  , txsModelFiles = [txsFilePath specDir "DisPro06a-datapos-nohide"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

test8 :: TxsExample
test8 = emptyExample
  { exampleName = "Process 07"
  , txsModelFiles = [txsFilePath specDir "DisPro07-gcd"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

test9 :: TxsExample
test9 = emptyExample
  { exampleName = "Process 08"
  , txsModelFiles = [txsFilePath specDir "DisPro08-gcdpurp"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

test10 :: TxsExample
test10 = emptyExample
  { exampleName = "Process 09"
  , txsModelFiles = [txsFilePath specDir "DisPro09-multiproc"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

test11 :: TxsExample
test11 = emptyExample
  { exampleName = "Process 10 - Data"
  , txsModelFiles = [txsFilePath specDir "DisPro10-data"]
  , txsCmdsFiles = [dispatchProcessCmdsPath]
  , expectedResult = Pass
  }

-- test12 :: TxsExample
-- test12 = emptyExample
--   { exampleName = "Process 12 - Unique ID"
--   , txsModelFiles = [txsFilePath specDir "DisPro12-unique-id"]
--   , txsCmdsFiles = [txsCmdPath exampDir "DispatchProcess_Stepper_Model_12"]
--   , expectedResult = Pass
--   }

examples :: [TxsExample]
examples = [ test0, test1, test2, test3, test4, test5, test6, test7, test8
           , test9, test10, test11 -- , test12
           ]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Dispatch Process #model" examples
