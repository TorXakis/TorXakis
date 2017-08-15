module Examples.AllSpec (spec) where

import           Examples.All
import           Sqatt
import           Test.Hspec

spec :: Spec
spec = beforeAll
         (checkSMTSolvers >> checkCompilers)
         (testExamples allExamples)
