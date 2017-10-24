{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module ExploreModels.CustomersOrders (exampleSet) where

-- Disabled these tests until CustomersOrders model is fixed. See #324
-- import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

-- customersOrdersText :: Text
-- customersOrdersText = "CustomersOrders"

-- exampDir :: FilePath
-- exampDir = "CustomersOrders"

-- test :: TxsExample
-- test = TxsExample
--   { exampleName = "Stepper 50"
--   , txsModelFiles = [txsFilePath exampDir customersOrdersText]
--   , txsCmdsFiles = [txsCmdPath exampDir "CustomersOrders_Stepper_Model"]
--   , txsServerArgs = []
--   , sutExample = Nothing
--   , expectedResult = Pass
--   }

examples :: [TxsExample]
examples = [] -- [test]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Customers and Orders #model" examples
