{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
import           ExploreModels.All
import           Sqatt           hiding (parallel)
import           Test.Hspec

main :: IO ()
main = do
    cd $ ".." </> ".."
    logDir <- mkLogDir "explore-model-test-"
    hspec $ parallel $ testExampleSets logDir allTests
