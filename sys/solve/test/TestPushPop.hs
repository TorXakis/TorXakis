{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module TestPushPop
(
testPushPopList
)
where

import           Control.Arrow       ((&&&))
import           Control.Monad.State
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Text           as T
import           Test.HUnit

import           ConstDefs
import           SMT
import           SMTData
import           SolveDefs
import           Sort
import           TestSolvers
import           ValExpr
import           VarId

testPushPopList :: Test
testPushPopList =
    TestList $ concatMap (\s -> map (\e -> TestLabel (fst e) $ TestCase $ do smtEnv <- createSMTEnv s False
                                                                             evalStateT (snd e) smtEnv )
                                    labelTestList
                         )
                         defaultSMTProcs

labelTestList :: [(String, SMT())]
labelTestList = [
        ("Push",                                        testPush),
        ("Push Assertion",                              testPushAssertion),
        ("Push Assertion Pop",                          testPushAssertionPop),
        ("Push Assertion Pop Assertion",                testPushAssertionPopAssertion),
        ("Nested Pusp Pop",                             testNestedRanges)
        ]

-----------------------------------------------------
-- Helper function
-----------------------------------------------------
getValues :: [VarId] -> SMT [Const]
getValues vs = do
    resp <- getSolvable
    lift $ assertEqual "sat" Sat resp
    sol <- getSolution vs
    let mValues = map (`Map.lookup` sol) vs
    lift $ assertBool "Is Just" (all isJust mValues)
    let values = map fromJust mValues
--    Trace.trace ("values = " ++ (show values)) $
    return values

testPushPopTemplate :: ([VarId] -> [([VarId] -> [ValExpr VarId], [Const] -> SMT())] -> SMT())
                    -> ADTDefs
                    -> [Sort]
                    -> ([VarId] -> [ValExpr VarId], [Const] -> SMT())
                    -> [([VarId] -> [ValExpr VarId], [Const] -> SMT())]
                    -> SMT()
testPushPopTemplate steps aDefs types (createInitialAssertions,checkInitial) pps = do
    _ <- SMT.openSolver
    addADTDefinitions aDefs
    let vs = map (\(x,t) -> VarId (T.pack ("i" ++ show x)) x t) (zip [1000..] types)
    addDeclarations vs
    addAssertions (createInitialAssertions vs)
    initialValues <- getValues vs
    checkInitial initialValues
    steps vs pps
    popValues <- getValues vs
    checkInitial popValues
    SMT.close

pushAssertionsCheck :: [VarId] -> ([VarId] -> [ValExpr VarId]) -> ([Const] -> SMT()) -> SMT()
pushAssertionsCheck vs createAssertions check = do
        push
        addAssertions (createAssertions vs)
        pushValues <- getValues vs
        check pushValues

testNestedPushPopTemplate :: ADTDefs -> [Sort] -> ([VarId] -> [ValExpr VarId], [Const] -> SMT()) ->
                                              [([VarId] -> [ValExpr VarId], [Const] -> SMT())] -> SMT()
testNestedPushPopTemplate = testPushPopTemplate nestedSteps
  where
    nestedSteps :: [VarId] -> [([VarId] -> [ValExpr VarId], [Const] -> SMT())] -> SMT()
    nestedSteps _ [] = return ()
    nestedSteps vs ((createAssertions, check):xs) = do
        pushAssertionsCheck vs createAssertions check
        nestedSteps vs xs
        pop

testSequentialPushPopTemplate :: ADTDefs -> [Sort] -> ([VarId] -> [ValExpr VarId], [Const] -> SMT()) ->
                                              [([VarId] -> [ValExpr VarId], [Const] -> SMT())] -> SMT()
testSequentialPushPopTemplate = testPushPopTemplate (mapM_ . step)
    where
        step :: [VarId] -> ([VarId] -> [ValExpr VarId], [Const] -> SMT()) -> SMT()
        step vs (createAssertions, check) = do
            pushAssertionsCheck vs createAssertions check
            pop

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
checkInt :: [Const] -> SMT()
checkInt [value] = case value of
    Cint _ -> lift $ assertBool "expected pattern" True
    _      -> lift $ assertBool "unexpected pattern" False
checkInt _  = error "One variable in problem"


testPush :: SMT ()
testPush = testSequentialPushPopTemplate emptyADTDefs [SortInt] (const [], checkInt) []

testPushAssertion :: SMT()
testPushAssertion = testSequentialPushPopTemplate
                        emptyADTDefs
                        [SortInt]
                        (const [], checkInt)
                        [(createAssertions, checkAssert)]
    where
        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v] = [cstrLT (cstrVar v) (cstrConst (Cint 0))]
        createAssertions _   = error "One variable in problem"

        checkAssert :: [Const] -> SMT()
        checkAssert [value] = case value of
            Cint x  -> lift $ assertBool ("expected pattern " ++ show x) (x < 0)
            _       -> lift $ assertBool "unexpected pattern" False
        checkAssert _       = error "One variable in problem"

testPushAssertionPop :: SMT()
testPushAssertionPop = testSequentialPushPopTemplate
                            emptyADTDefs
                            [SortInt]
                            (const [], checkInt)
                            [(createAssertions, checkAssert)]
    where
        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v] = [cstrGT (cstrVar v) (cstrConst (Cint 123))]
        createAssertions _   = error "One variable in problem"

        checkAssert :: [Const] -> SMT()
        checkAssert [value] = case value of
            Cint x  -> lift $ assertBool ("expected pattern " ++ show x) (x>123)
            _       -> lift $ assertBool "unexpected pattern" False
        checkAssert _       = error "One variable in problem"

testPushAssertionPopAssertion :: SMT()
testPushAssertionPopAssertion = testSequentialPushPopTemplate emptyADTDefs
                                                              [SortInt]
                                                              (const [], checkInt)
                                                              [(createAssertions1, checkAssert1),(createAssertions2, checkAssert2)]
    where
        createAssertions1 :: [VarId] -> [ValExpr VarId]
        createAssertions1 [v] = [cstrLT (cstrVar v) (cstrConst (Cint 0))]
        createAssertions1 _   = error "One variable in problem"

        checkAssert1 :: [Const] -> SMT()
        checkAssert1 [value]    = case value of
            Cint x -> lift $ assertBool ("expected pattern " ++ show x) (x<0)
            _      -> lift $ assertBool "unexpected pattern" False
        checkAssert1 _          = error "One variable in problem"

        createAssertions2 :: [VarId] -> [ValExpr VarId]
        createAssertions2 [v] = [cstrGT (cstrVar v) (cstrConst (Cint 0))]
        createAssertions2 _   = error "One variable in problem"

        checkAssert2 :: [Const] -> SMT()
        checkAssert2 [value]    = case value of
            Cint x -> lift $ assertBool ("expected pattern " ++ show x) (x>0)
            _      -> lift $ assertBool "unexpected pattern" False
        checkAssert2 _      = error "One variable in problem"

testNestedRanges :: SMT()
testNestedRanges = testNestedPushPopTemplate emptyADTDefs
                                             [SortInt]
                                             (const [], checkInt)
                                             (map (createAssertions Control.Arrow.&&& checkAssert) [100,99..1])
    where
        y = 123
        createAssertions :: Integer -> [VarId] -> [ValExpr VarId]
        createAssertions n [v] = [ cstrGT (cstrVar v) (cstrConst (Cint (y-n)))
                                 , cstrLT (cstrVar v) (cstrConst (Cint (y+n)))
                                 ]
        createAssertions _ _   = error "One variable in problem"

        checkAssert :: Integer -> [Const] -> SMT()
        checkAssert n [value] = case value of
            Cint x  -> lift $ assertBool ("expected pattern:\nx = " ++ show x ++ "\ny = " ++ show y ++ "\nn= " ++ show n) (y-n < x && x < y+n)
            _       -> lift $ assertBool "unexpected pattern" False
        checkAssert _ _       = error "One variable in problem"
