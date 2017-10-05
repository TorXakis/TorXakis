{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- | Test for all the examples used in the benchmarks. We want to be sure that
-- the examples are sane before running the benchmarks, hence the need for
-- these tests.
import           Benchmarks.All
import           Sqatt          hiding (parallel)
import           Test.Hspec

main :: IO ()
main = do
    cd $ ".." </> ".."
    logDir <- mkLogDir "benchmarks-test-"
    cd $ "test" </> "sqatt"
    hspec $ parallel $ testExampleSets (".." </> ".." </> logDir) allExamples
