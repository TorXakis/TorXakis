{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | This module defines the options that can be passed to TorXakis.
module Config
  ( Config (..)
  , getProc
  , defaultConfig
  , SolverId (..)
  , defaultSMTProcs
  , cvc4DefaultProc
  , z3DefaultProc
  )
where

import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe
import           System.Process

data SolverConfig = SolverConfig
  {
    -- | Executable name.
    execName :: FilePath
    -- | Arguments for to be passed to the smtSolver.
  , smtArgs  :: [String]
  } deriving (Eq, Show)

newtype SolverId = SolverId { solverId :: String }
  deriving (Eq, Ord, Show)

-- | TorXakis configuration options.
data Config = Config
  { -- | Log all SMT commands?
    smtLog           :: Bool
    -- | Available solvers that can be chosen from.
  , availableSolvers :: Map.Map SolverId SolverConfig
  , selectedSolver   :: SolverId
  } deriving (Eq, Show)

-- | TorXakis default configuration
defaultConfig :: Config
defaultConfig = Config
  { smtLog = False
  , availableSolvers = Map.fromList
                       [ (z3default, z3defaultConfig)
                       , (cvc4default, cvc4defaultConfig)
                       ]
  , selectedSolver = z3default
  }

z3default :: SolverId
z3default = SolverId "z3"

z3defaultConfig :: SolverConfig
z3defaultConfig = SolverConfig
  { execName = "z3"
  , smtArgs =
      [ "-smt2"
      , "-in"
      ]
  }

cvc4default :: SolverId
cvc4default = SolverId "cvc4"

cvc4defaultConfig :: SolverConfig
cvc4defaultConfig = SolverConfig
  { execName = "cvc4"
  , smtArgs =
      [ "--lang=smt"
      , "--incremental"
      , "--strings-exp"
      , "--fmf-fun-rlv"
      , "--uf-ss-fair"
      , "--no-strings-std-ascii"
      ]
  }

-- | Return a `CreateProcess` according to the selected solver in the
-- configuration.
getProc :: Config -> Maybe CreateProcess
getProc cfg = do
  solver <- Map.lookup (selectedSolver cfg) (availableSolvers cfg)
  return $ proc (execName solver) (smtArgs solver)

-- | Default SMT solvers. These are used only in test code.
--
-- TODO: check if this can't be placed somewhere else, or whether it is
-- necessary.
defaultSMTProcs :: [CreateProcess]
defaultSMTProcs =
  [ cvc4DefaultProc
  , z3DefaultProc
  ]

cvc4DefaultProc :: CreateProcess
cvc4DefaultProc = fromJust $ getProc defaultConfig { selectedSolver = cvc4default }

z3DefaultProc :: CreateProcess
z3DefaultProc = fromJust $ getProc defaultConfig { selectedSolver = z3default }
