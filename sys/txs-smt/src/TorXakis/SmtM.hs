{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Assertions
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the SMT ProblemSolver instance.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TorXakis.SmtM
( SmtM (..)
, SmtState (..)
)
where

import           Control.Monad.State
import           Control.Monad.Except
import           System.IO
import           System.Process

import           TorXakis.Error
import           TorXakis.ProblemSolver

-- | Smt State
data  SmtState = SmtState { inHandle         :: Handle
                          , outHandle        :: Handle
                          , errHandle        :: Handle
                          , smtProcessHandle :: ProcessHandle
                          , logFileHandle    :: Maybe Handle
                          , depth            :: Integer
                          }

-- | Smt Monad
newtype SmtM a = SmtM { -- | Run the Smt solver
                        runSmt :: StateT SmtState (ExceptT Error IO) a
                      }
                      deriving (Functor, Applicative, Monad, MonadState SmtState, MonadError Error)

instance ProblemSolver SmtM where
    depth = gets TorXakis.SmtM.depth
{-
-- ----------------------------------------------------------------------------------------- --
-- SMT state monad for smt solver

data EnvDefs = EnvDefs { sortDefs   :: Map.Map SortId SortDef
                       , cstrDefs   :: Map.Map CstrId CstrDef
                       , funcDefs   :: Map.Map FuncId (FuncDef VarId)
                       }
               deriving (Eq,Ord,Read,Show)

data EnvNames = EnvNames { sortNames   :: Map.Map SortId Text
                         , cstrNames   :: Map.Map CstrId Text
                         , funcNames   :: Map.Map FuncId Text
                         }
                deriving (Eq,Ord,Read,Show)

data  SmtEnv  =  SmtEnv     { inHandle          :: Handle
                            , outHandle         :: Handle
                            , errHandle         :: Handle
                            , smtProcessHandle  :: ProcessHandle
                            , logFileHandle     :: Maybe Handle
                            , envNames          :: EnvNames
                            , envDefs           :: EnvDefs
                            }
               | SmtEnvError


instance Show SmtEnv where
  show smtEnv =  show $ envNames smtEnv
-- ----------------------------------------------------------------------------------------- --
-- satSolve :  is set of constraints solvable?
-- solve    :  solve set of constraints and provide solution (as provided by smt solver), if satisfied
-- uniSolve :  solve set of constraints and provide solution, if uniquely satisfied
--             Unsat   :  0 solutions
--             Sat     :  1 solution
--             Unknown :  Unknown or >1 solutions

satSolve :: (Variable v) => [v] -> Assertions v -> SMT SolvableProblem
satSolve _  (Assertions AssertFalse)                = return Unsat
satSolve _  (Assertions (AssertSet s)) | Set.null s = return Sat
satSolve vs (Assertions (AssertSet s))              = 
    let vexps = Set.toList s in
        let vs' = List.nub $ vs ++ concatMap freeVars vexps in
            valExprsSat vs' vexps

solve :: (Variable v) => [v] -> Assertions v -> SMT (SolveProblem v)
solve _  (Assertions AssertFalse)                  = return Unsolvable
solve [] (Assertions (AssertSet s))   | Set.null s = return $ Solved Map.empty
solve vs (Assertions (AssertSet s))                = 
    let vexps = Set.toList s in
        let vs' = List.nub $ vs ++ concatMap freeVars vexps in do
            sp    <- valExprsSolve vs' vexps
            return $ case sp of 
                    Solved sol    -> Solved $ Map.filterWithKey (\k _ -> k `elem` vs) sol
                    Unsolvable    -> Unsolvable
                    UnableToSolve -> UnableToSolve 

uniSolve :: (Variable v) => [v] -> Assertions v -> SMT (SolveProblem v)
uniSolve _  (Assertions AssertFalse)                    = return Unsolvable
uniSolve []   (Assertions (AssertSet s))   | Set.null s = return $ Solved Map.empty
uniSolve vs (Assertions (AssertSet s))                  = 
    let vexps = Set.toList s in
        let vs' = List.nub $ vs ++ concatMap freeVars vexps in do
            sp    <- valExprsSolve vs' vexps
            case sp of
                Solved sol   | Map.null sol      -> return $ Solved sol
                Solved sol                       -> do sp' <- valExprsSat vs' (negateSolution sol:vexps)
                                                       return $ case sp' of
                                                            Sat     -> UnableToSolve
                                                            Unsat   -> Solved $ Map.filterWithKey (\k _ -> k `elem` vs) sol
                                                            Unknown -> UnableToSolve
                Unsolvable                       -> return Unsolvable
                UnableToSolve                    -> return UnableToSolve




-- function to "Reduce duplication"
valExprsSat' :: (Variable v) => [v] -> [ValExpr v] -> SMT SolvableProblem
valExprsSat' vs vexps =   do
    push
    addDeclarations vs
    addAssertions vexps
    getSolvable
    
valExprsSat :: (Variable v) => [v] -> [ValExpr v] -> SMT SolvableProblem
valExprsSat vs vexps  = do
    sat <- valExprsSat' vs vexps
    pop                                    -- restored, so no update needed
    return sat

valExprsSolve :: (Variable v) => [v] -> [ValExpr v] -> SMT (SolveProblem v)
valExprsSolve vs vexps  =  do
    sat <- valExprsSat' vs vexps  
    sp <- case sat of 
              { Sat     -> Solved . Map.filterWithKey (\k _ -> k `elem` vs) <$> getSolution vs
              ; Unsat   -> return Unsolvable
              ; Unknown -> return UnableToSolve 
              }         
    pop                                   -- restored, so no update needed
    return sp

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
-}