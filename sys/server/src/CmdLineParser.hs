{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See license.txt
-}

module CmdLineParser
  ( optsP -- Options parser exported for testing purposes only.
  , CmdLineConfig (..)
  , parseCmdLine
  )
where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Monoid (mempty)

-- imports from `core`
import Config
import Network

-- | Configuration options read by the command line.
data CmdLineConfig = CmdLineConfig
  { clSmtSolver :: !SMTSolver
  , clSmtLog :: !Bool
  , clPortNumber :: !PortNumber
  } deriving (Eq, Show)

parseCmdLine :: IO CmdLineConfig
parseCmdLine = do
  execParser opts
  where opts =
          info (optsP <**> helper)
               ( fullDesc
               <> progDesc "TorXakis server."
               )

optsP :: Parser CmdLineConfig
optsP = CmdLineConfig <$> smtSolverP <*> smtLogP <*> portP

smtSolverP :: Parser SMTSolver
smtSolverP = option auto
             ( long "smt-solver"
             <> help "SMT solver to be used"
             <> showDefault
             <> value Z3
             <> metavar "SOLVER" )

smtLogP :: Parser Bool
smtLogP = switch
          ( long "smt-log"
          <> help "Log the SMT output?"
          )

portP :: Parser PortNumber
portP = argument auto (metavar "PORT")
