{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Examples.Benchmarks.Sequence (benchmarksSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Sequence"

do100acts :: TxsExample
do100acts = TxsExample
    { exampleName = "100 actions"
    , txsModelFiles = [txsFilePathBench benchDir "SingleActionSequence"]
    , txsCommandsFile = txsCmdPathBench benchDir "SingleActionSequence"
    , sutExample = Nothing
    , expectedResult = Pass
    }


-- TODO: give the benchmark set a better structure, so that we can group the
-- benchmarks better.
benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Sequence" [do100acts]
