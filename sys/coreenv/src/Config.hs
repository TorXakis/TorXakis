{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | This module defines the options that can be passed to TorXakis.
module Config
  ( Config (..)
  , SMTSolver (..)
  )
where

-- | SMT Solvers that can be choosen.
data SMTSolver = Z3 | CVC4 deriving (Eq, Show, Read)

-- | TorXakis configuration options.
data Config = Config
  { -- | SMT solver
    smtSolver :: !SMTSolver
    -- | Log all SMT commands.
  , smtLog    :: !Bool
  } deriving (Eq, Show)
