-- | All the TorXakis examples that are part of the development acceptance test
-- combined.
module Examples.All
  ( allExamples
  )
where

import qualified Examples.Echo             as Echo
import qualified Examples.StimulusResponse as StimulusResponse
import           Sqatt

allExamples :: [TxsExampleSet]
--allExamples = [StimulusResponse.exampleSet, Echo.exampleSet]
allExamples = [StimulusResponse.exampleSet]
