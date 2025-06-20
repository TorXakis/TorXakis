{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module ExploreModels.All (allTests) where

import qualified ExploreModels.ControlLoop          as ControlLoop
import qualified ExploreModels.CustomersOrders      as CustomersOrders
import qualified ExploreModels.DispatchProcess      as DispatchProcess
import qualified ExploreModels.LuckyPeople          as LuckyPeople
-- Test disabled because it does not terminate. Tested on latest develop, v0.9 and v0.6, 7-9-24
-- import qualified ExploreModels.MovingArms           as MovingArms
import qualified ExploreModels.Queue                as Queue
import qualified ExploreModels.ReadWriteConflict    as ReadWriteConflict
import           Sqatt

allTests :: [TxsExampleSet]
allTests = [ ControlLoop.exampleSet
           , CustomersOrders.exampleSet
           , DispatchProcess.exampleSet
           , LuckyPeople.exampleSet
        --    , MovingArms.exampleSet
           , Queue.exampleSet
           , ReadWriteConflict.exampleSet
           ]
