{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.PushPopSpec
( spec
)
where

import           Control.Monad.Except
import           Control.Monad.State
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit

import           TorXakis.TestSolvers

import           TorXakis.ContextVar
import           TorXakis.Name
import           TorXakis.ProblemSolver
import           TorXakis.SmtM
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.Var

testPushPopList :: Test
testPushPopList =
    TestList $ concatMap (\s -> map (\e -> TestLabel (show s ++ ": " ++ show (fst e))
                                                     $ TestCase $ do
                                                                            es <- uncurry mkSmtState s False
                                                                            case es of
                                                                                Left err -> error (show err)
                                                                                Right ss -> do
                                                                                            r <- runExceptT $ execStateT (toStateT (snd e)) ss
                                                                                            case r of
                                                                                                Left err  -> error (show err)
                                                                                                Right ss' -> do
                                                                                                                me <- destroySmtState ss'
                                                                                                                case me of
                                                                                                                    Just err -> error (show err)
                                                                                                                    Nothing  -> return ()
                                    )
                                    labelTestList
                         )
                         defaultSMTProcs

labelTestList :: [(String, SmtM ())]
labelTestList =
        [ ("Push",                                        testPush)
        , ("Pop",                                         testPop)
        , ("Pop Illegal",                                 testPopIllegal)
        , ("Push and Pop isolate",                        testIsolate)
        ]

testPush :: ProblemSolver p => p ()
testPush = do
    currentDepth <- depth
    newDepth <- push
    liftIO $ assertEqual "push increases depth" newDepth (currentDepth+1)

testPop :: ProblemSolver p => p ()
testPop = do
    pushDepth <- push
    popDepth <- pop
    liftIO $ assertEqual "pop decreases depth" popDepth (pushDepth-1)

testPopIllegal :: SmtM ()
testPopIllegal =
        do
            _ <- pop
            liftIO $ assertFailure "pop not allowed in initial state - depth == 0"
        `catchError` handler
    where
        handler :: ProblemSolver p => Error -> p ()
        handler _ = return ()

testIsolate :: ProblemSolver p => p ()
testIsolate =
    let ctxIn = TorXakis.ContextVar.empty
        Right nm = mkName "var"
        ref :: RefByName VarDef
        ref = RefByName nm
        Right varDecl = mkVarDef ctxIn nm SortInt
        Right ctxOut = addVars [varDecl] ctxIn
        Right varExpr = mkVar ctxOut ref

        Right zero = mkConst ctxOut (Cint 0)
        Right gT = mkGT ctxOut varExpr zero
        Right lT = mkLT ctxOut varExpr zero
      in do
        _ <- push
        declareVariables [varDecl]

        -- individual
        
        _ <- push
        addAssertions [gT]
        respI <- solvable
        liftIO $ assertEqual "x > 0" (SolvableProblem (Just True)) respI
        _ <- pop

        _ <- push
        addAssertions [lT]
        respII <- solvable
        liftIO $ assertEqual "x < 0" (SolvableProblem (Just True)) respII
        _ <- pop
        
        _ <- push
        addAssertions [gT, lT]
        respIII <- solvable
        liftIO $ assertEqual "x > 0 && x <0" (SolvableProblem (Just False)) respIII
        _ <- pop
        
        -- nested gT first
        _ <- push
        addAssertions [gT]
        resp1 <- solvable
        liftIO $ assertEqual "x > 0" (SolvableProblem (Just True)) resp1

        _ <- push
        addAssertions [lT]
        resp2 <- solvable
        liftIO $ assertEqual "x > 0 && x < 0" (SolvableProblem (Just False)) resp2
        _ <- pop

        resp3 <- solvable
        liftIO $ assertEqual "x > 0" (SolvableProblem (Just True)) resp3
        _ <- pop

        -- nested lT first
        _ <- push
        addAssertions [lT]
        respA <- solvable
        liftIO $ assertEqual "x < 0" (SolvableProblem (Just True)) respA
        
        _ <- push
        addAssertions [gT]
        respB <- solvable
        liftIO $ assertEqual "x > 0 && x < 0" (SolvableProblem (Just False)) respB
        _ <- pop

        respC <- solvable
        liftIO $ assertEqual "x < 0" (SolvableProblem (Just True)) respC
        _ <- pop

        return ()

spec :: Spec
spec = describe "push pop" $
            fromHUnitTest testPushPopList