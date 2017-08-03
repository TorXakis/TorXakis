module Examples.StimulusResponseSpec (spec) where

import           Examples.StimulusResponse
import           Sqatt
import           Test.Hspec

spec :: Spec
spec = beforeAll
         (checkSMTSolvers >> checkCompilers)
         (testExamples Examples.StimulusResponse.examples)
