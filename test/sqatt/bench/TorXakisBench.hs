{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
import           Benchmarks.All
import           Criterion.Main
import           Turtle
main :: IO ()
main = do 
    cd $ ".." </> ".."
    defaultMain allBenchmarks

