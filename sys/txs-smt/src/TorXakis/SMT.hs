{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.SMT
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the SMT implementation of the ProblemSolver class.
-----------------------------------------------------------------------------
module TorXakis.SMT
( createSMTEnv
, openSolver
, close
, addDefinitions
, addDeclarations
, addAssertions
, getSolvable
, getSolution
, push
, pop
, put
, putT
, valExprToString
)
where

import           SMTInternal
