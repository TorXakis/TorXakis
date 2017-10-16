{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Benchmarks.All (allExamples, allBenchmarks) where

import qualified Benchmarks.Choice          as Choice
import qualified Benchmarks.Enable          as Enable
import qualified Benchmarks.Hiding          as Hiding
import qualified Benchmarks.Parallel        as Parallel
import qualified Benchmarks.RealWorld       as RealWorld
import qualified Benchmarks.Sequence        as Sequence
import qualified Benchmarks.Synchronization as Synchronization
import           Criterion.Main
import           Sqatt

allExamples :: [TxsExampleSet]
allExamples = [ Choice.benchmarksSet
              , Enable.benchmarksSet
              , Hiding.benchmarksSet
              , Parallel.benchmarksSet
              , RealWorld.benchmarksSet
              , Sequence.benchmarksSet
              , Synchronization.benchmarksSet
              ]

allBenchmarks :: [Benchmark]
allBenchmarks = benchmarkExampleSet <$> allExamples
