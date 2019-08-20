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
-- This module provides the ProblemSolver class.
-----------------------------------------------------------------------------
module TorXakis.ProblemSolver
( Solution (..)
, SolvableProblem (..)
, SolveProblem (..)
, KindOfProblem (..)
, ProblemSolver (..)
, negateSolution
)
where

import           Data.Either
import           Data.HashMap
import           Data.List

import           TorXakis.ContextValExpr
import           TorXakis.Value
import           TorXakis.ValExpr
import           TorXakis.Var

-- ----------------------------------------------------------------------------------------- --
-- Problem solving definitions

-- | Is Problem Solvable? i.e. does a solution exist?
newtype SolvableProblem = SolvableProblem { -- | to Maybe Bool: `Nothing` to handle limitations of the problem solver.
                                            toMaybeBool :: Maybe Bool }

-- | Solution
newtype  Solution = Solution { -- | to Map from Variable Name and Value
                               toMap :: Map (RefByName VarDef) Value }
    deriving (Eq, Ord, Read, Show)

-- | Solve Problem, i.e. give a solution
-- Include `UnableToSolve` to enable for limitation of the problem solver.
data  SolveProblem = Solved Solution
                   | Unsolvable
                   | UnableToSolve
     deriving (Eq, Ord, Read, Show)

-- | Kind of problem
data KindOfProblem = NoSolution
                   | UniqueSolution
                   | MultipleSolutions
     deriving (Eq, Ord, Read, Show)

-- | The Problem Solver class.
class ProblemSolver p where
    -- | Add Sorts
    -- precondition: `depth` == 0
    addSorts :: [Sort] -> p ()
    -- | Add Functions
    -- precondition: `depth` == 0
    addFunctions :: [FuncDef] -> p ()

    -- | depth of nested contexts
    -- postcondition: `depth` >= 0
    depth :: p Integer
    -- | push: add new nested context
    -- return new depth
    push :: p Integer
    -- | pop: remove deepest nested context
    -- precondition: `depth` > 0
    -- return new depth
    pop :: p depth
    
    -- | Declare Variables to current nested context.
    declareVariables :: [VarDef] -> p ()
    -- | add Assertions to current nested context.
    addAssertions :: [ValExpression] -> p()
    
    -- | is Problem Solvable?
    solvable :: p SolvableProblem
    -- | solve Problem.
    solve :: p SolveProblem
    -- | What is the kind of problem?
    kindOfProblem :: p KindOfProblem
    
    -- | conversion
    toValExprContext :: p ContextValExpr

-- | Boolean value exppression that negates the provided solution.
negateSolution :: VarContext c => c -> Solution -> Either Error ValExpression
negateSolution c sol = case partitionEithers [ mkVar c v >>= (\x -> mkConst c w >>= mkEqual c x)
                                             | (v,w) <- Data.HashMap.toList (toMap sol)
                                             ] of
                            ( [], vs ) -> Right vs
                            ( es, _ )  -> error ("unexpected errors in negateSolutions " ++ intercalate "\n" (Prelude.map show es))
                        >>= mkAnd c
                        >>= mkNot c
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
