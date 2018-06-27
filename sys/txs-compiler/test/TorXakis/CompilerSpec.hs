{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
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

import           Control.Lens            ((^?))
import           Data.Either             (isRight)
import           Data.Foldable           (traverse_)
import           System.FilePath         ((</>))
import           System.FilePath.Find    (extension, find, (==?))
import           Test.Hspec              (Spec, describe, expectationFailure,
                                          it, parallel, runIO, shouldBe,
                                          shouldSatisfy)
import           Text.RawString.QQ       (r)

import           TorXakis.Compiler       (compileFile, compileString)
import           TorXakis.Compiler.Error

spec :: Spec
spec = do
    describe "Compiles the orthogonal examples" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "success")
        parallel $ traverse_ checkSuccess fs
    describe "Compiles the examples in the 'examps' folder" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "examps")
        parallel $ traverse_ checkSuccess fs
    describe "Compiles large models" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
                           ("test" </> "data" </> "large")
        parallel $ traverse_ checkSuccess fs
    describe "Compiles large models" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
                           ("test" </> "data" </> "large")
        parallel $ traverse_ checkSuccess fs
    -- Failure test cases
    describe "Reports the expected errors " $
        parallel $ traverse_ checkFailure failureTestCases

    where
        checkSuccess fp = it (show fp) $ do
            res <- compileFile fp
            res `shouldSatisfy` isRight
        checkFailure (testName, snippet, expectation) = it testName $ do
            res <- compileString snippet
            case res of
                Right _ -> expectationFailure $
                    "Did not get the expected error "  ++ show expectation
                Left err -> err ^? errorType `shouldBe` Just expectation

failureTestCases :: [(TestName, CodeSnippet, ErrorType)]
failureTestCases = [ duplicatedParam ]

type TestName = String
type CodeSnippet = String

duplicatedParam :: (TestName, CodeSnippet, ErrorType)
duplicatedParam =
    ( "Function with two `x` parameters."
     , [r|
FUNCDEF myFunc(x, x :: Int) :: Int ::= x ENDDEF
        |]
     , MultipleDefinitions Variable
     )
