{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See LICENSE
-}

-- | This module defines the TorXakis options that.
module Config
  ( Config (..)
  , smtSolver
  , SMTSolver (..)
  , load
  , interpret
  )
where

import Data.Either
import Network (PortNumber)

-- | SMT Solvers that can be choosen.
data SMTSolver = Z3 | CVC4 

-- | TorXakis configuration options.
data Config = Config
  { smtSolver :: !SMTSolver
  , smtLog :: !Bool  -- ^ Log all SMT commands.
  , portNumber :: !PortNumber
  }

-- | Uninterpreted configuration options.
data UnintConfig = UnintConfig
  { mSmtSolver :: !(Maybe SMTSolver)
  , mSmtLog :: !(Maybe Bool)
  , mPortNumber :: !(Maybe PortNumber)
  }

type Error = String

-- TODO: change `Either` to `Validation`.
interpret :: UnintConfig -> Either [Error] Config
interpret = undefined

-- | Load the configuration options. These options can be specified by
-- different means:
--
--     1. Command line arguments.
--     2. Configuration file.
--     3. Environment variables.
--
-- The configuration options are searched in the order specified, and the first
-- option found is used.
load :: IO UnintConfig
load = undefined


-- | File name to look for.
configFileName = "txs.yaml"

-- | Create a `UnintConfig` value by trying to read the configuration options
-- given in a configuration file. The configuration file is assumed to be named
-- as defined by the variable `configFileName`.
--
-- This function looks for the configuration file in the following places:
--
--     1. The current working directory.
--     2. The home directory
--
-- The search proceeds in the order listed above, and it stops as soon as a
-- configuration file is found.
--
loadFromFile :: IO UnintConfig
loadFromFile = undefined
