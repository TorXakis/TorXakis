{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

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
