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
  , SolverConfig (..)
  , ParameterName (..)
  , ParameterValue (..)
  , changeSolver
  , changeLog
  , addSolvers
  , setParameters
  , updateCfg
  )
where

import qualified Data.Map       as Map
import           Data.Monoid
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

newtype ParameterName = ParamName { getParamName :: String }
  deriving (Eq, Ord, Show)

newtype ParameterValue = ParamValue { getParamValue :: String }
  deriving (Eq, Ord, Show)

-- | TorXakis configuration options.
data Config = Config
  { -- | Log all SMT commands?
    smtLog           :: Bool
    -- | Available solvers that can be chosen from.
  , availableSolvers :: Map.Map SolverId SolverConfig
  , selectedSolver   :: SolverId
  , parameters       :: Map.Map ParameterName ParameterValue
  } deriving (Eq, Show)

changeSolver :: Config -> String -> Config
changeSolver cfg solver = cfg { selectedSolver = SolverId solver }

changeLog :: Config -> Bool -> Config
changeLog cfg b = cfg { smtLog = b }

addSolvers :: Config -> Map.Map SolverId SolverConfig -> Config
addSolvers cfg newSolvers = cfg { availableSolvers = newSolvers <> availableSolvers cfg }

setParameters :: Config -> Map.Map ParameterName ParameterValue -> Config
setParameters cfg newParams = cfg { parameters = newParams }

updateCfg :: Maybe a -> (Config -> a -> Config) -> Config -> Config
updateCfg ma f cfg = maybe cfg (f cfg) ma

-- | TorXakis default configuration
defaultConfig :: Config
defaultConfig = Config
  { smtLog = False
  , availableSolvers = Map.fromList
                       [ (z3default, z3defaultConfig)
                       , (cvc4default, cvc4defaultConfig)
                       ]
  , selectedSolver = z3default
  , parameters = Map.fromList []
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
