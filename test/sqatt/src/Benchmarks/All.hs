module Benchmarks.All (allBenchmarks) where

import qualified Benchmarks.Choice   as Choice
import qualified Benchmarks.Sequence as Sequence
import           Criterion.Main
import           Sqatt

allBenchmarks :: [Benchmark]
allBenchmarks = benchmarkExampleSet <$> [ Sequence.benchmarksSet
                                        , Choice.benchmarksSet
                                        ]
