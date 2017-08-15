{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
-- | All the TorXakis examples that are part of the development acceptance test
-- combined.

module Examples.All (allExamples) where

import qualified Examples.StimulusResponse as StimulusResponse
import           Sqatt

allExamples :: [TxsExample]
allExamples = StimulusResponse.examples
