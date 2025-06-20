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
  , ParamName (..)
  , ParamValue (..)
  , changeSolver
  , changeLog
  , addSolvers
  , setParams
  -- * update values in a Map of Params with a list of configured parameters
  , updateParamVals
  , updateCfg
  )
where

import qualified Data.Map       as Map
import           System.Process

import           ParamCore

data SolverConfig = SolverConfig
  {
    -- | Executable name.
    execName :: FilePath
    -- | Arguments for to be passed to the smtSolver.
  , smtArgs  :: [String]
  } deriving (Eq, Show)

newtype SolverId = SolverId { solverId :: String }
  deriving (Eq, Ord, Show)

newtype ParamName = ParamName String deriving (Eq, Ord, Show)

newtype ParamValue = ParamValue String deriving (Eq, Ord, Show)

-- | TorXakis configuration options.
data Config = Config
  { -- | Log all SMT commands?
    smtLog           :: Bool
    -- | Available solvers that can be chosen from.
  , availableSolvers     :: Map.Map SolverId SolverConfig
  , selectedSolver       :: SolverId
  , configuredParameters :: [(ParamName,ParamValue)]
  } deriving (Eq, Show)

changeSolver :: Config -> String -> Config
changeSolver cfg solver = cfg { selectedSolver = SolverId solver }

changeLog :: Config -> Bool -> Config
changeLog cfg b = cfg { smtLog = b }

addSolvers :: Config -> Map.Map SolverId SolverConfig -> Config
addSolvers cfg newSolvers = cfg { availableSolvers = newSolvers <> availableSolvers cfg }

setParams :: Config -> [(ParamName,ParamValue)] -> Config
setParams cfg newParams = cfg { configuredParameters = newParams }

updateCfg :: Maybe a -> (Config -> a -> Config) -> Config -> Config
updateCfg ma f cfg = maybe cfg (f cfg) ma

-- | Updates a Map of parameters' values with given list of configured parameter
updateParamVals :: Params -- ^ Map of parameters ( Map.Map String (String,String->Bool) )
                -> [(ParamName, ParamValue)] -- ^ List of configured parameters
                -> Params
updateParamVals = foldl applyConfParam
  where applyConfParam :: Params
                       -> (ParamName, ParamValue)
                       -> Params
        applyConfParam allParams (ParamName pnStr, ParamValue pvStr) =
          Map.adjust (updateOldParamWith pvStr) ("param_" ++ pnStr) allParams
          where updateOldParamWith newValCandidate (oldVal, validate)
                  | validate newValCandidate  = (newValCandidate, validate)
                  | otherwise                 = (         oldVal, validate)

-- | TorXakis default configuration
defaultConfig :: Config
defaultConfig = Config
  { smtLog = False
  , availableSolvers = Map.fromList
                       [ (z3default, z3defaultConfig)
                       , (cvc4default, cvc4defaultConfig)
                       ]
  , selectedSolver = z3default
  , configuredParameters = []
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
      [ "--lang=smt2.5"
      , "--incremental"
      , "--strings-exp"
      , "--fmf-fun-rlv"
      , "--uf-ss-fair"
      , "--no-strings-print-ascii"
      ]
  }

-- | Return a `CreateProcess` according to the selected solver in the
-- configuration.
getProc :: Config -> Maybe CreateProcess
getProc cfg = do
  solver <- Map.lookup (selectedSolver cfg) (availableSolvers cfg)
  return $ proc (execName solver) (smtArgs solver)
