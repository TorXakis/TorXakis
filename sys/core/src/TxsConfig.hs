{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See LICENSE
-}

-- | This module defines the TorXakis options that can be set via a
-- configuration files, and provides utilities for reading such files.
module TxsConfig
  ( TxsConfig
  , smtSolver
  , SMTSolver (..)
  , loadConfig
  )
where

-- | SMT Solvers that can be choosen.
data SMTSolver = Z3 | CVC4 

-- | TorXakis configuration options.
data TxsConfig = TxsConfig
  { smtSolver :: SMTSolver
  , smtLog :: Bool  -- ^ Log all SMT commands.
  }

-- | File name to look for.
configFileName = "txs.config"

-- | Create a TxsConfig value by trying to read the configuration options given
-- in a configuration file. The configuration file is assumed to be named as
-- defined by the variable `configFileName`.
--
-- If in a configuration file certain options are missing then default options
-- are used.
--
-- This function looks for the configuration file in the following places:
--
--     1. The current working directory.
--     2. The home directory
--
-- The search proceeds in the order listed above, and it stops as soon as a
-- configuration file is found. If no configuration file is found, then the
-- default options are used.
--
loadConfig :: IO TxsConfig
loadConfig = undefined
