{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
import           Benchmarks.All
import           Criterion.Main

main :: IO ()
main = defaultMain allBenchmarks

