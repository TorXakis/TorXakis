{-# LANGUAGE OverloadedStrings #-}
module Examples.AllSpec (spec) where

import           Examples.All
import           Filesystem.Path
import           Sqatt
import           Test.Hspec
import           Turtle.Prelude

spec :: Spec
spec = beforeAll
         ( do cd $ ".." </> ".."
              checkSMTSolvers
              checkCompilers
              checkTxsInstall
         )
         (testExampleSets allExamples)
