{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- | Common definitions for benchmarks.
module Benchmarks.Common (benchCommonDir, seedSetupCmdFile) where

import           Filesystem.Path
import           Paths
import           Prelude         hiding (FilePath)

-- | Directory name of the common files for benchmarks.
benchCommonDir :: FilePath
benchCommonDir = "Common"

seedSetupCmdFile :: FilePath
seedSetupCmdFile = txsCmdPath BenchTest benchCommonDir "seedSetup"
