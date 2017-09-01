{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | All the TorXakis examples that are part of the development acceptance test
-- combined.
module Examples.All
  ( allExamples
  )
where

import qualified Examples.Adder            as Adder
import qualified Examples.ControlLoop      as ControlLoop
import qualified Examples.CustomersOrders  as CustomersOrders
import qualified Examples.DispatchProcess  as DispatchProcess
import qualified Examples.Echo             as Echo
import qualified Examples.LuckyPeople      as LuckyPeople
import qualified Examples.MovingArms       as MovingArms
import qualified Examples.Point            as Point
import qualified Examples.Queue            as Queue
import qualified Examples.StimulusResponse as StimulusResponse
import           Sqatt

allExamples :: [TxsExampleSet]
allExamples = [ Adder.exampleSet
              , ControlLoop.exampleSet
              , CustomersOrders.exampleSet
              , DispatchProcess.exampleSet
              , Echo.exampleSet
              , LuckyPeople.exampleSet
              , MovingArms.exampleSet
              , Point.exampleSet
              , Queue.exampleSet
              , StimulusResponse.exampleSet
              ]
