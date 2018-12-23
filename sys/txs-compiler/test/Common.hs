{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Common
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Common functionality for the parser and compiler tests.
--------------------------------------------------------------------------------

module Common (txsFilesIn, checkParallel, onAllFilesIn) where

import           Data.Foldable        (traverse_)
import           System.FilePath.Find (extension, find, (==?))
import           Test.Hspec (runIO, Spec, parallel)
import           Test.Hspec.Core (SpecWith, SpecM)

txsFilesIn :: FilePath -> SpecM a [FilePath]
txsFilesIn fp =runIO $
  find (return True) (extension ==? ".txs") fp

checkParallel :: (FilePath -> SpecM a r) -> [FilePath] -> SpecWith a
checkParallel fpTest = parallel . traverse_ fpTest

onAllFilesIn :: (FilePath -> SpecM a r) -> FilePath -> SpecWith a
onAllFilesIn test dirPath =
  txsFilesIn dirPath >>= checkParallel test
