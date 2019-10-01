{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.SymbolicSolver
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the Symbolic Solver.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
module TorXakis.SymbolicSolver
( SymbolicM (..)
, mkSymbolicState
, SymbolicState
)
where
import           Control.Monad.State
import qualified Data.HashMap

import           TorXakis.ContextSort
import           TorXakis.ProblemSolver
import           TorXakis.ValExpr
import           TorXakis.Value

-- | Symbolic State
newtype SymbolicState = SymbolicState { assertionsStack :: [ValExpression] }

-- | constructor for initial Symbolic State
mkSymbolicState :: SymbolicState
mkSymbolicState = SymbolicState [trueExpression]

-- | true Val Expression constructor
trueExpression :: ValExpression
trueExpression = case mkConst TorXakis.ContextSort.empty (mkBool True) of
                        Left e -> error ("SymbolicSolver: unexpectedly failed to create trueExpression with " ++ show e)
                        Right x -> x

-- | Symbolic Solver Monad
newtype SymbolicM p a = SymbolicM { -- | to `StateT`
                                    toStateT :: StateT SymbolicState p a
                                  }
                               deriving (Functor, Applicative, Monad, MonadState SymbolicState, MonadIO, MonadTrans)


instance ProblemSolver p => ProblemSolver (SymbolicM p) where
    info = do
                s <- lift info
                return $ "Symbolic Solver with " ++ s

    addADTs as = lift $ addADTs as

    addFunctions fs = lift $ addFunctions fs

    depth = lift depth

    push = do
            d <- lift push
            st <- get
            put st { assertionsStack = trueExpression : assertionsStack st }
            return d

    pop = do
            d <- lift pop
            st <- get
            put st { assertionsStack = tail (assertionsStack st) }
            return d

    declareVariables vs = lift $ declareVariables vs

    addAssertions as = do
            lift $ addAssertions as
            st <- get
            case assertionsStack st of
                (hd: tl) -> do
                                ctx <- toValExprContext
                                case mkAnd ctx (hd:as) of
                                    Left e -> error ("SymbolicSolver: addAssertions - mkAnd unexpectedly failed with " ++ show e)
                                    Right expr -> put st { assertionsStack = expr : tl }
                _        -> error "SymbolicSolver: addAssertions - assertionStack has unexpectedly no element"

    solvable = do
                st <- get
                ctx <- toValExprContext
                case mkAnd ctx (assertionsStack st) of
                        Left e -> error ("SymbolicSolver: solvable - mkAnd unexpectedly failed with " ++ show e)
                        Right expr -> case TorXakis.ValExpr.view expr of
                                        Vconst (TorXakis.Value.view -> Cbool b) -> return $ SolvableProblem (Just b)
                                        _                                       -> lift solvable

    solvePartSolution vs = do
                st <- get
                ctx <- toValExprContext
                case mkAnd ctx (assertionsStack st) of
                        Left e -> error ("SymbolicSolver: solvePartSolution - mkAnd unexpectedly failed with " ++ show e)
                        Right expr -> case TorXakis.ValExpr.view expr of
                                        Vconst (TorXakis.Value.view -> Cbool False)          -> return Unsolvable
                                        Vconst (TorXakis.Value.view -> Cbool True) | null vs -> return $ Solved (Solution Data.HashMap.empty)
                                        _                                                    -> lift $ solvePartSolution vs

    toValExprContext = lift toValExprContext

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
