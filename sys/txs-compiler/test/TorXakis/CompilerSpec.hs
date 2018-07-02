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
                    "Compilation succeeded instead of getting the expected error "
                    ++ show expectation
                Left err -> err ^? errorType `shouldBe` Just expectation

failureTestCases :: [(TestName, CodeSnippet, ErrorType)]
failureTestCases = [ duplicatedFuncParam1
                   , duplicatedFuncParam2
                   , duplicatedProcParam1
                   , duplicatedProcParam2
                   , duplicatedProcChan1
                   , duplicatedProcChan2
                   , duplicatedChan1
                   , duplicatedChan2
                   , duplicatedChan3
                   , duplicatedFunc1
                   , duplicatedFunc2
                   , duplicatedProc1
                   , duplicatedProc2
                   ]

type TestName = String
type CodeSnippet = String

duplicatedFuncParam1 :: (TestName, CodeSnippet, ErrorType)
duplicatedFuncParam1 =
    ( "Function with two `x` parameters. Variant 1."
     , [r|
FUNCDEF myFunc(x, x :: Int) :: Int ::= x ENDDEF
        |]
     , MultipleDefinitions Variable
     )

duplicatedFuncParam2 :: (TestName, CodeSnippet, ErrorType)
duplicatedFuncParam2 =
    ( "Function with two `x` parameters. Variant 2."
     , [r|
FUNCDEF myFunc(x :: Int ; x :: Int) :: Int ::= x ENDDEF
        |]
     , MultipleDefinitions Variable
     )

duplicatedProcParam1 :: (TestName, CodeSnippet, ErrorType)
duplicatedProcParam1 =
    ( "ProcDef with two `x` parameters. Variant 1."
     , [r|
PROCDEF myProc[H:: Int](x, x :: Int) ::= STOP ENDDEF
        |]
     , MultipleDefinitions Variable
     )

duplicatedProcParam2 :: (TestName, CodeSnippet, ErrorType)
duplicatedProcParam2 =
    ( "ProcDef with two `x` parameters. Variant 2."
     , [r|
PROCDEF myProc[H:: Int](x :: Int; x :: Int) ::= STOP ENDDEF
        |]
     , MultipleDefinitions Variable
     )

duplicatedProcChan1 :: (TestName, CodeSnippet, ErrorType)
duplicatedProcChan1 =
    ( "ProcDef with two `H` channel parameters. Variant 1."
     , [r|
PROCDEF myProc[H, H:: Int]() ::= STOP ENDDEF
        |]
     , MultipleDefinitions Channel
     )

duplicatedProcChan2 :: (TestName, CodeSnippet, ErrorType)
duplicatedProcChan2 =
    ( "ProcDef with two `H` channel parameters. Variant 2."
     , [r|
PROCDEF myProc[H :: Int; J :: Bool; H:: Int]() ::= STOP ENDDEF
        |]
     , MultipleDefinitions Channel
     )

duplicatedChan1 :: (TestName, CodeSnippet, ErrorType)
duplicatedChan1 =
    ( "ChannelDef with two `In` channels. Variant 1."
     , [r|
CHANDEF myChans ::= In, In :: Int ENDDEF
        |]
     , MultipleDefinitions Channel
     )

duplicatedChan2 :: (TestName, CodeSnippet, ErrorType)
duplicatedChan2 =
    ( "ChannelDef with two `In` channels. Variant 2."
     , [r|
CHANDEF myChans ::= In :: Int; In :: Int ENDDEF
        |]
     , MultipleDefinitions Channel
     )

duplicatedChan3 :: (TestName, CodeSnippet, ErrorType)
duplicatedChan3 =
    ( "Two ChannelDefs that both define an `In` channel. Variant 3."
     , [r|
CHANDEF myChans   ::= In :: Int ENDDEF
CHANDEF yourChans ::= In :: Int ENDDEF
        |]
     , MultipleDefinitions Channel
     )

duplicatedFunc1 :: (TestName, CodeSnippet, ErrorType)
duplicatedFunc1 =
    ( "Function with same signature twice. Variant 1."
     , [r|
FUNCDEF myFunc(x :: Int) :: Int ::= x ENDDEF
FUNCDEF myFunc(y :: Int) :: Int ::= y ENDDEF
        |]
     , MultipleDefinitions Function
     )

duplicatedFunc2 :: (TestName, CodeSnippet, ErrorType)
duplicatedFunc2 =
    ( "Function with same signature twice. Variant 2."
     , [r|
FUNCDEF myFunc(x :: Int; y :: Int) :: Int ::= x ENDDEF
FUNCDEF myFunc(a,b :: Int) :: Int ::= a ENDDEF
        |]
     , MultipleDefinitions Function
     )

duplicatedProc1 :: (TestName, CodeSnippet, ErrorType)
duplicatedProc1 =
    ( "Process with same signature twice. Variant 1."
     , [r|
PROCDEF myProc[G]()::= STOP ENDDEF
PROCDEF myProc[H]()::= STOP ENDDEF
        |]
     , MultipleDefinitions Process
     )


duplicatedProc2 :: (TestName, CodeSnippet, ErrorType)
duplicatedProc2 =
    ( "Process with same signature twice. Variant 2."
     , [r|
PROCDEF myProc[G, H :: Int       ]()::= STOP ENDDEF
PROCDEF myProc[A :: Int; B :: Int]()::= STOP ENDDEF
        |]
     , MultipleDefinitions Process
     )
