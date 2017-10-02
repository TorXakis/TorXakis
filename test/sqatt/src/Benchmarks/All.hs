module Benchmarks.All (allBenchmarks) where

import qualified Benchmarks.Choice          as Choice
import qualified Benchmarks.Sequence        as Sequence
import qualified Benchmarks.Synchronization as Synchronization
import           Criterion.Main
import           Sqatt

allBenchmarks :: [Benchmark]
allBenchmarks = benchmarkExampleSet <$> [ Choice.benchmarksSet
                                        , Sequence.benchmarksSet
                                        , Synchronization.benchmarksSet
                                        ]
