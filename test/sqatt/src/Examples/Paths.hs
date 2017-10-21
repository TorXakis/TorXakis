{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module specifies the location of the example files (models, commands,
-- and SUT's), and provides function for building the file-paths for TorXakis
-- models, commands, and SUT files.
module Examples.Paths
  ( txsCmdPath
  , txsFilePath
  , javaFilePath
  , txsPurposeFromTracePath
  )
where

import           Data.Text
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

-- | Directory where the examples are placed.
exampsDir :: FilePath
exampsDir = "examps"

-- | Directory where the tests are placed.
testsDir :: FilePath
testsDir = "test"</> "examps"

-- | Directory where the traces are placed.
traceDir :: FilePath
-- For now, traces are placed in the root folder of `TorXakis`.
traceDir = ""

-- | Make a TorXakis model file path. It appends the given directory to the
-- examples directory, and appends the .txs extension to the given file path.
txsFilePath :: FilePath -- ^ Directory of the current example.
            -> Text     -- ^ File name of the current example.
            -> FilePath
txsFilePath currExampDir fp =
  exampsDir </> currExampDir </> fromText fp <.> "txs"

-- | Make a TorXakis purpose file path, that was generated from a trace. It
-- assumes that the file is located in the trace directory `traceDir`. It
-- appends the .txs extension to the given file name.
txsPurposeFromTracePath :: Text     -- ^ File name of the current example.
            -> FilePath
txsPurposeFromTracePath fp =
  traceDir </> fromText fp <.> "txs"


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
