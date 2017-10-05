module Benchmarks.All (allExamples, allBenchmarks) where

import qualified Benchmarks.Choice          as Choice
import qualified Benchmarks.Enable          as Enable
import qualified Benchmarks.Hiding          as Hiding
import qualified Benchmarks.Parallel        as Parallel
import qualified Benchmarks.Sequence        as Sequence
import qualified Benchmarks.Synchronization as Synchronization
import           Criterion.Main
import           Sqatt

allExamples :: [TxsExampleSet]
allExamples = [ Choice.benchmarksSet
              , Enable.benchmarksSet
              , Hiding.benchmarksSet
              , Parallel.benchmarksSet
              , Sequence.benchmarksSet
              , Synchronization.benchmarksSet
              ]

allBenchmarks :: [Benchmark]
allBenchmarks = benchmarkExampleSet <$> allExamples
