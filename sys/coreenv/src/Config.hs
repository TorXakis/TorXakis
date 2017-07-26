{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See LICENSE
-}

-- | This module defines the TorXakis options that.
module Config
  ( Config (..)
  , SMTSolver (..)
  )
where

-- | SMT Solvers that can be choosen.
data SMTSolver = Z3 | CVC4 deriving (Show, Read)

-- | TorXakis configuration options.
data Config = Config
  { smtSolver :: !SMTSolver   -- ^ SMT solver
  , smtLog :: !Bool           -- ^ Log all SMT commands.
  }
