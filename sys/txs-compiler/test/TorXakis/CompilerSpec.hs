{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts #-}
-- We choose convenience over safety. It is ok if tests fail due to an incomplete pattern.

--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.CompilerSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests for the 'TorXakis' compiler.
--------------------------------------------------------------------------------
module TorXakis.CompilerSpec
    (spec)
where

import           Data.Either          (isRight)
import           Data.Foldable        (traverse_)
import           System.FilePath      ((</>))
import           System.FilePath.Find (extension, find, (==?))
import           Test.Hspec           (Spec, describe, describe, it, parallel,
                                       runIO, shouldSatisfy)

import           TorXakis.Compiler    (compileFile)

spec :: Spec
spec = do
    describe "Correctly compiles the orthogonal examples" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "success")
        parallel $ traverse_ testCompiler fs
    describe "Compiles the examples in the 'examps' folder" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "examps")
        parallel $ traverse_ testCompiler fs
    describe "Correctly compiles large models" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
                           ("test" </> "data" </> "large")
        parallel $ traverse_ testCompiler fs
    where
        testCompiler fp = it (show fp) $ do
            r <- compileFile fp
            r `shouldSatisfy` isRight
