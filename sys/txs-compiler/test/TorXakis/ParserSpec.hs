{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ParserSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests for the 'TorXakis' parser.
--------------------------------------------------------------------------------
module TorXakis.ParserSpec
    (spec)
where

import           Data.Either          (isRight)
import           Data.Foldable        (traverse_)
import           System.FilePath      ((</>))
import           System.FilePath.Find (extension, find, (==?))
import           Test.Hspec           (Spec, describe, it, parallel, runIO,
                                       shouldSatisfy)

import           TorXakis.Parser      (parseFile)


spec :: Spec
spec = do
    describe "Correctly parses the incremental examples (WIP)" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "success")
        parallel $ traverse_ testParser fs

    describe "Correctly parses complete TorXakis models" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "parser" </> "success")
        parallel $ traverse_ testParser fs

    describe "Correctly parses the HUGE models" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "parser" </> "huge")
        parallel $ traverse_ testParser fs

    where
        testParser fp = it (show fp) $ do
            r <- parseFile fp
            r `shouldSatisfy` isRight
