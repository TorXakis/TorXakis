{-# LANGUAGE OverloadedStrings #-}
module Examples.AllSpec (spec) where

import           Examples.All
import           Filesystem.Path
import           Sqatt
import           System.IO
import           Test.Hspec
import           Turtle.Prelude

spec :: Spec
spec = beforeAll
         ( do cd $ ".." </> ".."
              checkSMTSolvers
              checkCompilers
              checkTxsInstall
              hSetBuffering System.IO.stdout NoBuffering
         )
         (testExampleSets allExamples)
