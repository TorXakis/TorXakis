{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for working with tests paths within 'sqatt'.
module Paths
    ( BenchTest (BenchTest)
    , ITest (ITest)
    , txsFilePath
    , txsCmdPath
    )
where

import           Data.Text
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

-- | Types of tests supported by 'sqatt'.
class TestType a where
    -- | Directory that contains the model and command files that are used in
    -- the tests. This is relative to 'sqatt' root folder.
    dataDir :: a -> FilePath

-- | Test of the examples used in the benchmarks.
data BenchTest = BenchTest
instance TestType BenchTest where
    dataDir _ = "data" </> "bench"

-- | Integration test.
data ITest = ITest
instance TestType ITest where
    dataDir _ = "data" </> "integration-test"

-- | Make a TorXakis model file path. It appends the given directory to the
-- test data directory, and appends the '.txs extension to the given file path.
--
txsFilePath :: TestType a
            => a        -- ^ Test type. This will determine the location of the test data directory.
            -> FilePath -- ^ Directory of the current example.
            -> Text     -- ^ File name of the current example.
            -> FilePath
txsFilePath tt currExampDir fp =
  dataDir tt </> currExampDir </> fromText fp <.> "txs"

-- | Make a TorXakis commands file path. It appends the given directory to the
-- test data directory, and appends the .txscmd extension to the given file
-- path.
txsCmdPath :: TestType a
           => a        -- ^ Test type. This will determine the location of the test data directory.
           -> FilePath -- ^ Directory of the current example.
           -> Text     -- ^ File name of the current example.
           -> FilePath
txsCmdPath tt currExampDir fp =
  dataDir tt </> currExampDir </> fromText fp <.> "txscmd"
