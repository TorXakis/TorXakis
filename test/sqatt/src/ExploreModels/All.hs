{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module ExploreModels.All (allTests) where

import qualified ExploreModels.ControlLoop          as ControlLoop
import qualified ExploreModels.CustomersOrders      as CustomersOrders
import qualified ExploreModels.DispatchProcess      as DispatchProcess
import qualified ExploreModels.LuckyPeople          as LuckyPeople
import qualified ExploreModels.MovingArms           as MovingArms
import qualified ExploreModels.Queue                as Queue
import qualified ExploreModels.ReadWriteConflict    as ReadWriteConflict
import           Sqatt

allTests :: [TxsExampleSet]
allTests = [ ControlLoop.exampleSet
           , CustomersOrders.exampleSet
           , DispatchProcess.exampleSet
           , LuckyPeople.exampleSet
           , MovingArms.exampleSet
           , Queue.exampleSet
           , ReadWriteConflict.exampleSet
           ]
