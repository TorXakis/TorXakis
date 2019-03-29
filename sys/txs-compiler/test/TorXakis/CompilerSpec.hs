{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts #-}
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

--import           Control.Lens            ((^..))
--import           Data.Data.Lens          (biplate)
import           Data.Either             (isRight)
import           Data.Text               (unpack)
import           Debug.Trace
--import           Data.Foldable           (traverse_)
import           System.FilePath         ((</>))
import           Test.Hspec              (Spec, describe, 
                                          --expectationFailure,
                                          it,
                                          --parallel, 
                                          --shouldBe,
                                          shouldSatisfy)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
--import           Text.RawString.QQ       (r)

import           Common                  (onAllFilesIn)
import           TorXakis.Compiler       (compileFile, 
                                          compileString
                                          )
import           TorXakis.Language
import           TorXakis.PrettyPrint
import           TorXakis.SortContext
import           TorXakis.SortGenContext
import           TorXakis.OptionsGen

--import           TorXakis.Compiler.Error

spec :: Spec
spec = do
    describe "Compiles the single-concept examples" $
      checkSuccess `onAllFilesIn` ("test" </> "data" </> "success")

    describe "Compiles the examples in the 'examps' folder" $
      checkSuccess `onAllFilesIn` ("test" </> "data" </> "examps")

    describe "Compiles large models" $
      checkSuccess `onAllFilesIn` ("test" </> "data" </> "large")

    describe "Compiles the regression tests" $
      checkSuccess `onAllFilesIn` ("test" </> "data" </> "regression")

    describe "Round tripping of" $
        it "randomly generated data types" $ property roundTripping
    -- Failure test cases
    -- describe "Reports the expected errors " $
    --    parallel $ traverse_ checkFailure failureTestCases

    where
        checkSuccess fp = it (show fp) $ do
            res <- compileFile fp
            res `shouldSatisfy` isRight

        roundTripping :: Gen Property
        roundTripping = do
            OptionsGen o <- arbitrary
            ctx <- arbitraryTestSortContext
            return $ monadicIO $ do
                                    b <- run (checkCompile o ctx)
                                    assert b

        checkCompile :: Options -> ContextTestSort -> IO Bool
        checkCompile o ctx = let s = prettyPrintSortContext o ctx in do
                                 res <- compileString (unpack (toText s))
                                 case res of
                                      Right c -> -- trace ("\nChecking context:\n" ++ show s) $ 
                                                    return $ elemsADT ctx == elemsADT c
                                      Left e  -> trace ("Failure on context:\n" ++ show s ++ "\nerror: " ++ show e) (return False)
        -- checkFailure (testName, snippet, expectedErrs) = it testName $ do
            -- res <- compileString snippet
            -- case res of
                -- Right _ -> expectationFailure $
                    -- "Compilation succeeded instead of getting the expected errors "
                    -- ++ show expectedErrs
                -- Left err -> err ^.. biplate `shouldBe` expectedErrs

{-
failureTestCases :: [(TestName, CodeSnippet, [ErrorType])]
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
                   , duplicatedVarInActPrefix1
                   , duplicatedVarInLet1
                   ]
  where
    duplicatedFuncParam1 =
        ( "Function with two `x` parameters. Variant 1."
         , [r|
FUNCDEF myFunc(x, x :: Int) :: Int ::= x ENDDEF
            |]
         , [MultipleDefinitions Variable]
         )
    duplicatedFuncParam2 =
        ( "Function with two `x` parameters. Variant 2."
         , [r|
FUNCDEF myFunc(x :: Int ; x :: Int) :: Int ::= x ENDDEF
            |]
         , [MultipleDefinitions Variable]
         )
    duplicatedProcParam1 =
        ( "ProcDef with two `x` parameters. Variant 1."
         , [r|
PROCDEF myProc[H:: Int](x, x :: Int) ::= STOP ENDDEF
            |]
         , [MultipleDefinitions Variable]
         )
    duplicatedProcParam2 =
        ( "ProcDef with two `x` parameters. Variant 2."
         , [r|
PROCDEF myProc[H:: Int](x :: Int; x :: Int) ::= STOP ENDDEF
            |]
         , [MultipleDefinitions Variable]
         )
    duplicatedProcChan1 =
        ( "ProcDef with two `H` channel parameters. Variant 1."
         , [r|
PROCDEF myProc[H, H:: Int]() ::= STOP ENDDEF
            |]
         , [MultipleDefinitions Channel]
         )
    duplicatedProcChan2 =
        ( "ProcDef with two `H` channel parameters. Variant 2."
         , [r|
PROCDEF myProc[H :: Int; J :: Bool; H:: Int]() ::= STOP ENDDEF
            |]
         , [MultipleDefinitions Channel]
         )
    duplicatedChan1 =
        ( "ChannelDef with two `In` channels. Variant 1."
         , [r|
CHANDEF myChans ::= In, In :: Int ENDDEF
            |]
         , [MultipleDefinitions Channel]
         )
    duplicatedChan2 =
        ( "ChannelDef with two `In` channels. Variant 2."
         , [r|
CHANDEF myChans ::= In :: Int; In :: Int ENDDEF
            |]
         , [MultipleDefinitions Channel]
         )
    duplicatedChan3 =
        ( "Two ChannelDefs that both define an `In` channel. Variant 3."
         , [r|
CHANDEF myChans   ::= In :: Int ENDDEF
CHANDEF yourChans ::= In :: Int ENDDEF
            |]
         , [MultipleDefinitions Channel]
         )
    duplicatedFunc1 =
        ( "Function with same signature twice. Variant 1."
         , [r|
FUNCDEF myFunc(x :: Int) :: Int ::= x ENDDEF
FUNCDEF myFunc(y :: Int) :: Int ::= y ENDDEF
            |]
         , [MultipleDefinitions Function]
         )
    duplicatedFunc2 =
        ( "Function with same signature twice. Variant 2."
         , [r|
FUNCDEF myFunc(x :: Int; y :: Int) :: Int ::= x ENDDEF
FUNCDEF myFunc(a,b :: Int) :: Int ::= a ENDDEF
            |]
         , [MultipleDefinitions Function]
         )
    duplicatedProc1 =
        ( "Process with same signature twice. Variant 1."
         , [r|
PROCDEF myProc[G]()::= STOP ENDDEF
PROCDEF myProc[H]()::= STOP ENDDEF
            |]
         , [MultipleDefinitions Process]
         )
    duplicatedProc2 =
        ( "Process with same signature twice. Variant 2."
         , [r|
PROCDEF myProc[G, H :: Int       ]()::= STOP ENDDEF
PROCDEF myProc[A :: Int; B :: Int]()::= STOP ENDDEF
            |]
         , [MultipleDefinitions Process]
         )
    duplicatedVarInActPrefix1 =
        ( "Introducing duplicated variables in action prefix."
         , [r|
CHANDEF  MyChans
 ::=
    A,B :: Int
ENDDEF

MODELDEF M
 ::=
    CHAN IN
    CHAN OUT A,B

    SYNC {A},{B},{A|B}

    BEHAVIOUR
       { A ? x | B ? x } [[ x == 43 ]]  >->  B ! x  >->  STOP
ENDDEF
            |]
         --  We expect two errors to be reported, one per-each location in which
         --  @x@ appears in the action offer.
         , [MultipleDefinitions Variable, MultipleDefinitions Variable]
         )
    duplicatedVarInLet1 =
        ( "Introducing duplicated variables in let binding."
         , [r|
FUNCDEF let() :: Int ::=
    LET x :: Int = 10, x :: Int = 2 IN x NI
ENDDEF
           |]
        , [MultipleDefinitions Variable, MultipleDefinitions Variable]
        )

type TestName = String
type CodeSnippet = String
-}