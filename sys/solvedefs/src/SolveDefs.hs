{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
--
--   Module SolveDefs :  Some Definitions for Solving
--
-- ----------------------------------------------------------------------------------------- --


{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SolveDefs
( Solution
, SolvableProblem(..)
, SolveProblem(..)
)

where

import qualified Data.Map  as Map

import TxsDefs
import TxsShow


-- ----------------------------------------------------------------------------------------- --
-- SMT definitions


type  Solution v       =  Map.Map v Const

data  SolvableProblem  = Sat
                       | Unsat
                       | Unknown
     deriving (Eq,Ord,Read,Show)

data  SolveProblem v  = Solved (Solution v)
                      | Unsolvable
                      | UnableToSolve
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --

instance (Variable v, PShow v) => PShow (Solution v)
  where
    pshow sol
      =  concatMap (\(v,w) -> pshow v ++ " = " ++ pshow w ++ "\n") (Map.toList sol)


instance (Variable v) => PShow (SolveProblem v)

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
