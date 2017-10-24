{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module ExploreModels.All (allTests) where

import qualified ExploreModels.ControlLoop as ControlLoop
import qualified ExploreModels.CustomersOrders as CustomersOrders
import qualified ExploreModels.DispatchProcess as DispatchProcess
import qualified ExploreModels.LuckyPeople as LuckyPeople
import           Sqatt

allTests :: [TxsExampleSet]
allTests = [ ControlLoop.exampleSet
           , CustomersOrders.exampleSet
           , DispatchProcess.exampleSet
           , LuckyPeople.exampleSet
           ]
