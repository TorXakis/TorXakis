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

import           Control.Lens            ((^..))
import           Data.Data.Lens          (biplate)
import           Data.Either             (isRight)
import           Data.Foldable           (traverse_)
import           System.FilePath         ((</>))
import           Test.Hspec              (Spec, describe, expectationFailure,
                                          it, parallel, shouldBe,
                                          shouldSatisfy)
import           Text.RawString.QQ       (r)

import           Common                  (onAllFilesIn)
import           TorXakis.Compiler       (compileFile, compileString)
import           TorXakis.Compiler.Error

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

    -- Failure test cases
    describe "Reports the expected errors " $
        parallel $ traverse_ checkFailure failureTestCases

    where
        checkSuccess fp = it (show fp) $ do
            res <- compileFile fp
            res `shouldSatisfy` isRight
        checkFailure (testName, snippet, expectedErrs) = it testName $ do
            res <- compileString snippet
            case res of
                Right _ -> expectationFailure $
                    "Compilation succeeded instead of getting the expected errors "
                    ++ show expectedErrs
                Left err -> err ^.. biplate `shouldBe` expectedErrs

failureTestCases :: [(TestName, CodeSnippet, [ErrorType])]
failureTestCases = [ wrongChannelArguments1
                   , wrongChannelArguments2
                   , wrongChannelArguments3
                   , typeMismatch1
                   , typeMismatch2
                   , typeMismatch3
                   , typeMismatch4
                   , typeMismatch5
                   , typeMismatch6
                   , duplicatedFuncParam1
                   , duplicatedFuncParam2
                   , duplicatedProcParam1
                   , duplicatedProcParam2
                   , duplicatedProcChan1
                   , duplicatedProcChan2
                   , duplicatedChan1
                   , duplicatedChan2
                   , duplicatedChan3
                   , duplicatedChanInOffer
                   , duplicatedExitInOffer
                   , duplicatedFunc1
                   , duplicatedFunc2
                   , duplicatedProc1
                   , duplicatedProc2
                   , duplicatedVarInActPrefix1
                   , duplicatedVarInLet1
                   , duplicatedModelDefName
                   ]
  where
    wrongChannelArguments1 =
        ( "Wrong usage of Channel. Variant 1. More arguments than defined."
        , [r|
PROCDEF p [ A ] ( x :: Int ) ::=
    A ! x
ENDDEF
           |]
        , [Undefined Process]
        )
    wrongChannelArguments2 =
        ( "Wrong usage of Channel. Variant 2. Less arguments than defined."
        , [r|
PROCDEF p [ A :: Int ] ( ) ::=
    A
ENDDEF
           |]
        , [Undefined Process]
        )
    wrongChannelArguments3 =
        ( "Wrong usage of Channel. Variant 3. Different sort of argument than defined."
        , [r|
PROCDEF p [ A :: Int ] ( s :: String ) ::=
    A ! s
ENDDEF
           |]
        , [Undefined Process]
        )
    typeMismatch1 =
        ( "Type Mismatch. Variant 1. Different implicit sort of valexpression than explicitly defined in let variable."
        , [r|
PROCDEF p [ A :: Int ] ( x :: Int ) ::=
    LET a :: Int = "string" IN
        A ! a
    NI
ENDDEF
           |]
        , [Undefined Process]
        )
    typeMismatch2 =
        ( "Type Mismatch. Variant 2. Different implicit sort than explicitly defined. Argument of LET."
        , [r|
PROCDEF p [ A :: Int ] ( x :: Int ) ::=
    LET a = "string" :: Int IN
        A ! a
    NI
ENDDEF
           |]
        , [Undefined Process]
        )
    typeMismatch3 =
        ( "Type Mismatch. Variant 3. Different implicit sort than explicitly defined. Argument of Function."
        , [r|
FUNCDEF f ( x :: Int ) :: Int ::=
    f ( "string" :: Int )
ENDDEF
           |]
        , [Undefined Function]
        )
    typeMismatch4 =
        ( "Type Mismatch. Variant 4. Different implicit sort than explicitly defined. Nested argument of Function."
        , [r|
FUNCDEF f ( x :: Int ) :: Int ::=
    f ( 3 + "string" :: Int )
ENDDEF
           |]
        , [Undefined Function]
        )    
    typeMismatch5 =
        ( "Type Mismatch. Variant 5. Different implicit sort than explicitly defined. Argument of Process."
        , [r|
PROCDEF p [A] ( x :: String ) ::=
    p[A] ( 3 :: String )
ENDDEF
           |]
        , [Undefined Process]
        )
    typeMismatch6 =
        ( "Type Mismatch. Variant 6. Different implicit sort than explicitly defined. Nested argument of Process."
        , [r|
PROCDEF p [A] ( x :: String ) ::=
    p[A] ( "string" ++ 3 :: String )
ENDDEF
           |]
        , [Undefined Process]
        )    
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
    duplicatedChanInOffer = 
        ( "A channel occurs more than once in an offer"
         , [r|
PROCDEF p [A] () ::=
    A | A
ENDDEF
            |]
         , [Undefined Process]
         )
    duplicatedExitInOffer = 
        ( "An Exit occurs more than once in an offer"
         , [r|
PROCDEF p [A] () EXIT ::=
    EXIT | EXIT
ENDDEF
            |]
         , [Undefined Process]
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
    duplicatedModelDefName =
            ( "Duplicated modeldef name"
         , [r|
MODELDEF Mod ::=
    CHAN IN
    CHAN OUT
    BEHAVIOUR
            EXIT
ENDDEF

MODELDEF Mod ::=
    CHAN IN
    CHAN OUT
    BEHAVIOUR
            STOP
ENDDEF
           |]
        , [MultipleDefinitions Model]
        )

type TestName = String
type CodeSnippet = String
