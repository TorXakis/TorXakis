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
import           System.IO
import           Test.Hspec

spec :: Spec
spec = beforeAll
         ( do checkSMTSolvers
              checkCompilers
              checkTxsInstall
              hSetBuffering System.IO.stdout NoBuffering
         )
         ( do
             runIO $ cd $ ".." </> ".."
             logDir <- runIO $ mkLogDir "test-"
             testExampleSets logDir allExamples
         )
