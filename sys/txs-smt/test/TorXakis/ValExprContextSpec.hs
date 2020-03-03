{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExprContextSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'ValExprContext'.
-----------------------------------------------------------------------------
module TorXakis.ValExprContextSpec
(spec
)
where
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Either
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.ProblemSolver
import           TorXakis.SmtM
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.ValExprContext
import           TorXakis.Value
import           TorXakis.Var

import           TorXakis.ContextTestValExpr
import           TorXakis.TestSolvers
import           TorXakis.TestSortContext
import           TorXakis.ValueGen


-- | run All solvers
runSolvers :: SmtM Bool -> Expectation
runSolvers exec = do
        bs <- liftIO $ mapM (runSolver exec) [cmdZ3] -- defaultSMTProcs, issues in Cvc4 & Z3Str3
        bs `shouldSatisfy` and

-- | run specific solver
runSolver :: SmtM Bool -> (FilePath,[String]) -> IO Bool
runSolver exec (fp,as) = do
    liftIO $ threadDelay 500000 -- wait half a second, to prevent creating log files with identical time stamp:
                                -- uncaught exception: IOException of type ResourceBusy (logSMT.2019-09-27-16-45-21.7920361.smt2: openFile: resource busy (file is locked))
    es <- liftIO $ mkSmtState fp as False
    case es of
        Left err -> error (show err)
        Right ss -> do
                    res <- liftIO $ runExceptT $ -- smt Solver
                                      runStateT (TorXakis.SmtM.toStateT exec)
                                                 ss
                    case res of
                        Left err  -> error (show err)
                        Right (b,ss') -> do
                                        me <- liftIO $ destroySmtState ss'
                                        case me of
                                            Just err -> error (show err)
                                            Nothing  -> return b

-- | Solve equal function calls
testFunctionArbitrary :: (FuncContext c, ProblemSolver p) => c -> (Sort -> IO Value) -> p Bool
testFunctionArbitrary ctx genVal = do
    TorXakis.ProblemSolver.addADTs (elemsADT ctx)
    TorXakis.ProblemSolver.addFunctions (elemsFunc ctx)
    and <$> mapM checkFunc (elemsFunc ctx)
  where
    checkFunc :: ProblemSolver p => FuncDef -> p Bool
    checkFunc fd =
        let ps = paramDefs fd
            vars = toList ps
            ss = map (getSort ctx) vars
         in do
            vals <- liftIO $ mapM genVal ss
            case partitionEithers (map (mkConst ctx) vals) of
                ([], valExprs) -> do
                                    _ <- TorXakis.ProblemSolver.push
                                    TorXakis.ProblemSolver.declareVariables vars
                                    ctx' <- toValExprContext
                                    case partitionEithers (map (mkVar ctx' . RefByName . name) vars) of
                                        ([], varExprs) -> let fr = RefByFuncSignature $ getFuncSignature ctx' fd
                                                              Right fCallVar = mkFuncOpt ctx' fr varExprs
                                                              Right fCallVal = mkFuncOpt ctx' fr valExprs
                                                              Right eq = mkEqual ctx' fCallVal fCallVar
                                                            in 
                                                            case partitionEithers (zipWith (mkEqual ctx') varExprs valExprs) of
                                                                ([], eqs) -> do
                                                                                addAssertions (eq:eqs)
                                                                                res <- TorXakis.ProblemSolver.solvable
                                                                                _ <- TorXakis.ProblemSolver.pop
                                                                                case toMaybeBool res of
                                                                                    Nothing -> error ("Problem solver can't determine function call is equal for\n" ++ show fd ++ "\nwith\n" ++ show valExprs)
                                                                                    Just b  -> return b
                                                                (es, _)        -> error ("mkEqual unexpectely failed with " ++ show es)
                                        (es, _)        -> error ("mkVar unexpectely failed with " ++ show es)
                (es, _)        -> error ("mkConst unexpectely failed with " ++ show es)

-- | function calls are equal
prop_FuncCallEqual :: Gen Expectation
prop_FuncCallEqual = do
    n <- getSize
    ctx <- arbitraryContextTestValExpr
    return $ runSolvers (testFunctionArbitrary ctx (genVal n ctx))
  where
    genVal :: TestSortContext c => Int -> c -> Sort -> IO Value
    genVal n ctx = (generate . resize n) . arbitraryValueOfSort ctx  -- resize ensures that we are able to generate data values for the generated sorts

spec :: Spec
spec =
  describe "All Function Definitions" $
            it "are usable" $ property prop_FuncCallEqual
