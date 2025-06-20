{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module CmdLineParser
  ( optsP -- Options parser exported for testing purposes only.
  , CmdLineConfig (..)
  , parseCmdLine
  )
where

import           Options.Applicative

-- imports from `core`
import           Network.Socket (PortNumber)

-- | Configuration options read by the command line.
data CmdLineConfig = CmdLineConfig
  { clSmtSolver  :: Maybe String
  , clSmtLog     :: Maybe Bool
  , clPortNumber :: Maybe PortNumber
  } deriving (Eq, Show)

parseCmdLine :: IO CmdLineConfig
parseCmdLine = execParser opts
  where opts =
          info (optsP <**> helper)
               ( fullDesc
               <> progDesc "TorXakis server."
               )

optsP :: Parser CmdLineConfig
optsP = CmdLineConfig <$> smtSolverP <*> smtLogP <*> portP

smtSolverP :: Parser (Maybe String)
smtSolverP = optional $ strOption
             ( long "smt-solver"
             <> help ("SMT solver to be used. It must be selected from "
                     ++ "the list of available solvers.")
             <> metavar "SOLVER"
             )

smtLogP :: Parser (Maybe Bool)
smtLogP = optional $
      flag' True
        (long "smt-log" <> help "Log the SMT solver commands.")
  <|> flag' False
        (long "no-smt-log" <> help "Don't log the SMT solver commands.")

portP :: Parser (Maybe PortNumber)
portP = optional $ argument auto (metavar "PORT")
