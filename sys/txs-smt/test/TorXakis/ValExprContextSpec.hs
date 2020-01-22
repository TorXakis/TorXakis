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
import qualified Data.HashMap
import           Test.Hspec
import           Test.Hspec.QuickCheck
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
        bs <- liftIO $ mapM (runSolver exec) [cmdCVC4] -- defaultSMTProcs
                                                     -- found issues in cvc4 with this test case
                                                     -- https://github.com/CVC4/CVC4/issues/3316
                                                     -- https://github.com/CVC4/CVC4/issues/3317
                                                     -- Note cvc4's performance might remain an issue
                                                     -- Also Z3 can crash https://github.com/Z3Prover/z3/issues/2602
                                                     --         or be very slow https://github.com/Z3Prover/z3/issues/2601
        bs `shouldSatisfy` and

-- | run specific solver
runSolver :: SmtM Bool -> (FilePath,[String]) -> IO Bool
runSolver exec (fp,as) = do
    liftIO $ threadDelay 500000 -- wait half a second, to prevent creating log files with identical time stamp:
                                -- uncaught exception: IOException of type ResourceBusy (logSMT.2019-09-27-16-45-21.7920361.smt2: openFile: resource busy (file is locked))
    es <- liftIO $ mkSmtState fp as True
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
testFunctionArbitrary :: (FuncContext c, ProblemSolver p) => c -> Data.HashMap.Map Sort Value -> p Bool
testFunctionArbitrary ctx mp = do
    TorXakis.ProblemSolver.addADTs (elemsADT ctx)
    TorXakis.ProblemSolver.addFunctions (elemsFunc ctx)
    and <$> mapM checkFunc (elemsFunc ctx)
  where
    checkFunc :: ProblemSolver p => FuncDef -> p Bool
    checkFunc fd =
        let ps = paramDefs fd
            vars = toList ps
            vals = map ((Data.HashMap.!) mp . getSort ctx) vars
        in case partitionEithers (map (mkConst ctx) vals) of
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
                                                                                    Nothing -> error ("Problem solver can't determine function call is equal for " ++ show fd)
                                                                                    Just b  -> return b
                                                                (es, _)        -> error ("mkEqual unexpectely failed with " ++ show es)
                                        (es, _)        -> error ("mkVar unexpectely failed with " ++ show es)
                (es, _)        -> error ("mkConst unexpectely failed with " ++ show es)

-- | function calls are equal
prop_FuncCallEqual :: Gen Expectation
prop_FuncCallEqual = do
    ctx <- arbitraryContextTestValExpr
    mp <- foldM (addValue ctx) Data.HashMap.empty (elemsSort ctx)
    return $ runSolvers (testFunctionArbitrary ctx mp)
  where
    addValue :: TestSortContext c => c -> Data.HashMap.Map Sort Value -> Sort -> Gen (Data.HashMap.Map Sort Value)
    addValue ctx mp s = do
        v <- arbitraryValueOfSort ctx s
        return $ Data.HashMap.insert s v mp

spec :: Spec
spec =
  describe "All Function Definitions" $
    modifyMaxSize (const 10) $ -- limit due to limitations of z3 => see https://github.com/Z3Prover/z3/issues/2601
        it "are usable" $ property prop_FuncCallEqual
