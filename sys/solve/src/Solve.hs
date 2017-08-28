{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module Solve

-- ----------------------------------------------------------------------------------------- --
--
--   Module Solve :  Interface to SMT, Constraint, ... Solver
--
-- ----------------------------------------------------------------------------------------- --
-- export

( satSolve
, solve
, uniSolve
, randSolve
, Assertions
, Solve.empty
, add 
, SolveRandParam(..)
, toRandParam
)

-- ----------------------------------------------------------------------------------------- --
-- import

where
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import TxsDefs
import StdTDefs
import FreeVar

import SolveDefs
import SMTInternal
import SMTData

import RandPartition
import RandTrueBins
import RandIncrementChoice

import SolveRandParam
-- ----------------------------------------------------------------------------------------- --

data PrivateAssertions v  = AssertFalse
                          | AssertSet ( Set.Set (ValExpr v) )
     deriving (Eq,Ord,Read,Show)

newtype Assertions v = Assertions (PrivateAssertions v)
     deriving (Eq,Ord)

instance (Variable v) => Show (Assertions v) where
    show (Assertions pa) = show pa

empty :: Assertions v
empty = Assertions (AssertSet Set.empty)

add :: (Variable v) => ValExpr v -> Assertions v -> Assertions v
add e (Assertions AssertFalse)    |  sortOf e == sortId_Bool                                            = Assertions AssertFalse
add e a                           |  sortOf e == sortId_Bool  && (view e == Vconst (Cbool True))        = a
add e _                           |  sortOf e == sortId_Bool  && (view e == Vconst (Cbool False))       = Assertions AssertFalse
add e (Assertions (AssertSet s) ) |  sortOf e == sortId_Bool                                            = Assertions ( AssertSet (Set.insert e s) )
add e _                                                                                                 = error ("Add - Can not add non-boolean expression " ++ show e)

-- ----------------------------------------------------------------------------------------- --
-- satSolve :  is set of constraints solvable?
-- solve    :  solve set of constraints and provide solution (as provided by smt solver), if satisfied
-- uniSolve :  solve set of constraints and provide solution, if uniquely satisfied
--             Unsat   :  0 solutions
--             Sat     :  1 solution
--             Unknown :  Unknown or >1 solutions
-- TODO: improve interface of UNISOLVE -> NrOfSolutions := 0, 1, >1 and unknown


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

-- random solve
randSolve :: (Variable v) => SolveRandParam -> [v] -> Assertions v -> SMT (SolveProblem v)
randSolve _ _  (Assertions AssertFalse)                  = return Unsolvable
randSolve _ [] (Assertions (AssertSet s))   | Set.null s = return $ Solved Map.empty
randSolve p vs (Assertions (AssertSet s))                = 
    let vexps = Set.toList s in
        let vs' = List.nub $ vs ++ concatMap freeVars vexps in do
            sp    <- case p of
                        RandNo                  -> valExprsSolve vs' vexps                          -- allow easy comparison of difference in performance of randomization algorithm
                        RandPartition r         -> randValExprsSolvePartition r vs' vexps
                        RandTrueBins r          -> randValExprsSolveTrueBins r vs' vexps
                        RandIncrementChoice r   -> randValExprsSolveIncrementChoice r vs' vexps
            return $ case sp of 
                    Solved sol    -> Solved $ Map.filterWithKey (\k _ -> k `elem` vs) sol
                    Unsolvable    -> Unsolvable
                    UnableToSolve -> UnableToSolve 



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
              { Sat     -> do sol <- getSolution vs
                              return $ Solved $ Map.filterWithKey (\k _ -> k `elem` vs) sol
              ; Unsat   -> return Unsolvable
              ; Unknown -> return UnableToSolve 
              }         
    pop                                   -- restored, so no update needed
    return sp

-- ----------------------------------------------------------------------------------------- --

negateSolution :: (Variable v) => Solution v -> ValExpr v
negateSolution sol = cstrNot (cstrAnd (Set.fromList [ cstrEqual (cstrVar v) (cstrConst w) | (v,w) <- Map.toList sol ]) )

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
