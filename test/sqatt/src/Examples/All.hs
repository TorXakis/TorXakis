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
import qualified Examples.Echo             as Echo
import qualified Examples.LuckyPeople      as LuckyPeople
import qualified Examples.StimulusResponse as StimulusResponse
import           Sqatt

allExamples :: [TxsExampleSet]
allExamples = [ Adder.exampleSet
              , Echo.exampleSet
              , LuckyPeople.exampleSet
              , StimulusResponse.exampleSet
              ]
