{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-all #-}
-- TODO: remove 'OPTIONS_GHC' after finishing this task.
import           Criterion.Main
import           Criterion.Main.Options
import qualified Examples.Benchmarks.Sequence as Sequence
import           Sqatt
import           System.Environment

main :: IO ()
main = do
    logDir <- mkLogDir "bench-"
    matches <- getArgs -- For now we use the arguments to match as glob patterns
    runMode (Run defaultConfig Glob matches)
        [ benchmarkExampleSet logDir Sequence.benchmarksSet
        ]
