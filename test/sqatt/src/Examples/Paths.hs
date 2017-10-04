{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
-- | This module specifies the location of the example files, and provides
-- function for building the file-paths for TorXakis models, commands, and SUT
-- files.
module Examples.Paths
  ( txsCmdPath
  , txsFilePath
  , javaFilePath
  , txsFilePathBench
  , txsCmdPathBench
  )
where

import           Data.Text
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

-- | Directory that contains the model and command files that are used for the
-- benchmarks. This is relative to 'sqatt' root folder.
benchDataDir :: FilePath
benchDataDir = "data" </> "bench"

-- | Make a TorXakis model file path. It appends the given directory to the
-- benchmarks input files directory, and appends the .txs extension to the
-- given file path.
--
-- TODO: reduce this duplication.
txsFilePathBench :: FilePath -- ^ Directory of the current example.
                 -> Text     -- ^ File name of the current example.
                 -> FilePath
txsFilePathBench currExampDir fp =
  benchDataDir </> currExampDir </> fromText fp <.> "txs"

-- | Make a TorXakis commands file path. It appends the given directory to the
-- benchmarks input files directory, and appends the .txscmd extension to the
-- given file path.
txsCmdPathBench :: FilePath -- ^ Directory of the current example.
                -> Text     -- ^ File name of the current example.
                -> FilePath
txsCmdPathBench currExampDir fp =
  benchDataDir </> currExampDir </> fromText fp <.> "txscmd"


-- | Directory where the examples are placed.
exampsDir :: FilePath
exampsDir = "examps"

-- | Directory where the tests are placed.
testsDir :: FilePath
testsDir = "test"</> "examps"

-- | Make a TorXakis model file path. It appends the given directory to the
-- examples directory, and appends the .txs extension to the given file path.
txsFilePath :: FilePath -- ^ Directory of the current example.
            -> Text     -- ^ File name of the current example.
            -> FilePath
txsFilePath currExampDir fp =
  exampsDir </> currExampDir </> fromText fp <.> "txs"

-- | Make a TorXakis commands file path. It appends the given directory to the
-- test directory, and appends the .txscmd extension to the given file path.
txsCmdPath :: FilePath -- ^ Directory of the current example.
           -> Text     -- ^ File name of the current example.
           -> FilePath
txsCmdPath currExampDir fp =
  testsDir </> currExampDir </> fromText fp <.> "txscmd"

-- | Make a java file path. It appends the given directory to the examples
-- directory, and appends the .java extension to the given file path.
javaFilePath :: FilePath -- ^ Directory of the current example.
             -> Text     -- ^ File name of the current example.
             -> FilePath
javaFilePath currExampDir fp =
  exampsDir </> currExampDir </> fromText fp <.> "java"

