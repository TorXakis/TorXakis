{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.CustomersOrders (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

customersOrdersText :: Text
customersOrdersText = "CustomersOrders"

exampDir :: FilePath
exampDir = "CustomersOrders"

test :: TxsExample
test = TxsExample
  { exampleName = "CustomersOrders Test"
  , txsModelFile = txsFilePath exampDir customersOrdersText
  , txsCommandsFile = txsCmdPath exampDir "CustomersOrders_Tester"
  , sutExample =
    Just (JavaExample
           (javaFilePath exampDir customersOrdersText)
           []
         )
  , expectedResult = Pass
  }

examples :: [TxsExample]
examples = [test]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Customers and Orders" examples
