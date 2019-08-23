{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module TestPushPop
(
testPushPopList
)
where

--import           Control.Arrow       ((&&&))
import           Control.Monad.Except
import           Control.Monad.State
--import qualified Data.Map            as Map
--import           Data.Maybe
--import qualifid Data.Text           as T
import           Test.HUnit

-- import           TorXakis.Value
-- import           SortId
-- import           ValExpr
-- import           VarId

-- import           SMT
-- import           SMTData
-- import           SolveDefs
import           TestSolvers

import           TorXakis.Error
import           TorXakis.ProblemSolver
import           TorXakis.SmtM

testPushPopList :: Test
testPushPopList =
    TestList $ concatMap (\s -> map (\e -> TestLabel (fst e) $ TestCase $ do
                                                                            es <- uncurry mkSmtState s False
                                                                            case es of
                                                                                Left err -> error (show err)
                                                                                Right ss -> do
                                                                                            r <- runExceptT $ execStateT (runSmt (snd e)) ss
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
labelTestList = [ ("Nothing",                                     testNothing)
                , ("Solve True",                                  testSolveTrue)
                , ("Push",                                        testPush)
                , ("Pop",                                         testPop)
                , ("Pop Illegal",                                 testPopIllegal)
      --  ("Push Assertion",                              testPushAssertion),
      --  ("Push Assertion Pop",                          testPushAssertionPop),
      --  ("Push Assertion Pop Assertion",                testPushAssertionPopAssertion),
      --  ("Nested Pusp Pop",                             testNestedRanges)
        ]

testNothing :: SmtM ()
testNothing = return ()

testSolveTrue :: SmtM ()
testSolveTrue = do
                    s <- solvable
                    liftIO $ assertEqual "solvable expected" (SolvableProblem (Just True)) s

testPush :: SmtM ()
testPush = do
    currentDepth <- depth
    newDepth <- push
    liftIO $ assertEqual "push increases depth" newDepth (currentDepth+1)

testPop :: SmtM ()
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
        handler :: Error -> SmtM ()
        handler _ = return ()
    
{-
-----------------------------------------------------
-- Helper function
-----------------------------------------------------
getValues :: [VarId] -> SMT [Constant]
getValues vs = do
    resp <- getSolvable
    lift $ assertEqual "sat" Sat resp
    sol <- getSolution vs
    let mValues = map (`Map.lookup` sol) vs
    lift $ assertBool "Is Just" (all isJust mValues)
    let values = map fromJust mValues
--    Trace.trace ("values = " ++ (show values)) $
    return values

testPushPopTemplate :: ([VarId] -> [([VarId] -> [ValExpr VarId], [Constant] -> SMT())] -> SMT()) ->
                        EnvDefs -> [SortId] -> ([VarId] -> [ValExpr VarId], [Constant] -> SMT()) ->
                                              [([VarId] -> [ValExpr VarId], [Constant] -> SMT())] -> SMT()
testPushPopTemplate steps envDefs' types (createInitialAssertions,checkInitial) pps = do
    _ <- SMT.openSolver
    addDefinitions envDefs'
    let vs = map (\(x,t) -> VarId (T.pack ("i" ++ show x)) x t) (zip [1000..] types)
    addDeclarations vs
    addAssertions (createInitialAssertions vs)
    initialValues <- getValues vs
    checkInitial initialValues
    steps vs pps
    popValues <- getValues vs
    checkInitial popValues
    SMT.close

pushAssertionsCheck :: [VarId] -> ([VarId] -> [ValExpr VarId]) -> ([Constant] -> SMT()) -> SMT()
pushAssertionsCheck vs createAssertions check = do
        push
        addAssertions (createAssertions vs)
        pushValues <- getValues vs
        check pushValues

testNestedPushPopTemplate :: EnvDefs -> [SortId] -> ([VarId] -> [ValExpr VarId], [Constant] -> SMT()) ->
                                              [([VarId] -> [ValExpr VarId], [Constant] -> SMT())] -> SMT()
testNestedPushPopTemplate = testPushPopTemplate nestedSteps
  where
    nestedSteps :: [VarId] -> [([VarId] -> [ValExpr VarId], [Constant] -> SMT())] -> SMT()
    nestedSteps _ [] = return ()
    nestedSteps vs ((createAssertions, check):xs) = do
        pushAssertionsCheck vs createAssertions check
        nestedSteps vs xs
        pop

testSequentialPushPopTemplate :: EnvDefs -> [SortId] -> ([VarId] -> [ValExpr VarId], [Constant] -> SMT()) ->
                                              [([VarId] -> [ValExpr VarId], [Constant] -> SMT())] -> SMT()
testSequentialPushPopTemplate = testPushPopTemplate (mapM_ . step)
    where
        step :: [VarId] -> ([VarId] -> [ValExpr VarId], [Constant] -> SMT()) -> SMT()
        step vs (createAssertions, check) = do
            pushAssertionsCheck vs createAssertions check
            pop

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
checkInt :: [Constant] -> SMT()
checkInt [value] = case value of
    Cint _ -> lift $ assertBool "expected pattern" True
    _      -> lift $ assertBool "unexpected pattern" False
checkInt _  = error "One variable in problem"


testPush :: SMT ()
testPush = testSequentialPushPopTemplate (EnvDefs Map.empty Map.empty Map.empty) [sortIdInt]   (const [], checkInt) []

testPushAssertion :: SMT()
testPushAssertion = testSequentialPushPopTemplate (EnvDefs Map.empty Map.empty Map.empty) [sortIdInt]  (const [], checkInt)
                                                                                          [(createAssertions, checkAssert)]
    where
        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v] = [cstrLT (cstrVar v) (cstrConst (Cint 0))]
        createAssertions _   = error "One variable in problem"

        checkAssert :: [Constant] -> SMT()
        checkAssert [value] = case value of
            Cint x  -> lift $ assertBool ("expected pattern " ++ show x) (x < 0)
            _       -> lift $ assertBool "unexpected pattern" False
        checkAssert _       = error "One variable in problem"

testPushAssertionPop :: SMT()
testPushAssertionPop = testSequentialPushPopTemplate (EnvDefs Map.empty Map.empty Map.empty) [sortIdInt]  (const [], checkInt)
                                                                [(createAssertions, checkAssert)]
    where
        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v] = [cstrGT (cstrVar v) (cstrConst (Cint 123))]
        createAssertions _   = error "One variable in problem"

        checkAssert :: [Constant] -> SMT()
        checkAssert [value] = case value of
            Cint x  -> lift $ assertBool ("expected pattern " ++ show x) (x>123)
            _       -> lift $ assertBool "unexpected pattern" False
        checkAssert _       = error "One variable in problem"

testPushAssertionPopAssertion :: SMT()
testPushAssertionPopAssertion = testSequentialPushPopTemplate (EnvDefs Map.empty Map.empty Map.empty) [sortIdInt]  (const [], checkInt)
                                                                                      [(createAssertions1, checkAssert1),(createAssertions2, checkAssert2)]
    where
        createAssertions1 :: [VarId] -> [ValExpr VarId]
        createAssertions1 [v] = [cstrLT (cstrVar v) (cstrConst (Cint 0))]
        createAssertions1 _   = error "One variable in problem"

        checkAssert1 :: [Constant] -> SMT()
        checkAssert1 [value]    = case value of
            Cint x -> lift $ assertBool ("expected pattern " ++ show x) (x<0)
            _      -> lift $ assertBool "unexpected pattern" False
        checkAssert1 _          = error "One variable in problem"

        createAssertions2 :: [VarId] -> [ValExpr VarId]
        createAssertions2 [v] = [cstrGT (cstrVar v) (cstrConst (Cint 0))]
        createAssertions2 _   = error "One variable in problem"

        checkAssert2 :: [Constant] -> SMT()
        checkAssert2 [value]    = case value of
            Cint x -> lift $ assertBool ("expected pattern " ++ show x) (x>0)
            _      -> lift $ assertBool "unexpected pattern" False
        checkAssert2 _      = error "One variable in problem"

testNestedRanges :: SMT()
testNestedRanges = testNestedPushPopTemplate (EnvDefs Map.empty Map.empty Map.empty) [sortIdInt]  (const [], checkInt)
                                                                     (map (createAssertions Control.Arrow.&&& checkAssert) [100,99..1])
    where
        y :: Integer
        y = 123
        createAssertions :: Integer -> [VarId] -> [ValExpr VarId]
        createAssertions n [v] = [ cstrGT (cstrVar v) (cstrConst (Cint (y-n)))
                                 , cstrLT (cstrVar v) (cstrConst (Cint (y+n)))
                                 ]
        createAssertions _ _   = error "One variable in problem"

        checkAssert :: Integer -> [Constant] -> SMT()
        checkAssert n [value] = case value of
            Cint x  -> lift $ assertBool ("expected pattern:\nx = " ++ show x ++ "\ny = " ++ show y ++ "\nn= " ++ show n) (y-n < x && x < y+n)
            _       -> lift $ assertBool "unexpected pattern" False
        checkAssert _ _       = error "One variable in problem"
-}