{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- | Configuration related functionality for TorXakis server.
module TxsServerConfig (loadConfig, interpretConfig, Config (..), UnintConfig(..)) where

import           CmdLineParser
import qualified Config        as CoreConfig
import           Network


data Config = Config
  { smtSolver  :: !CoreConfig.SMTSolver
  , smtLog     :: !Bool
  , portNumber :: !PortNumber
  }

-- | Uninterpreted configuration options.
data UnintConfig = UnintConfig
  { mSmtSolver  :: !(Maybe CoreConfig.SMTSolver)
  , mSmtLog     :: !(Maybe Bool)
  , mPortNumber :: !(Maybe PortNumber)
  }

type Error = String

interpretConfig :: CmdLineConfig -> Either [Error] Config
interpretConfig cfg =
  Right Config
          { smtSolver = clSmtSolver cfg
          , smtLog = clSmtLog cfg
          , portNumber = clPortNumber cfg
          }

-- | Load the configuration options. These options can be specified by
-- different means:
--
--     1. Command line arguments.
--     2. Configuration file.
--     3. Environment variables.
--
-- The configuration options are searched in the order specified, and the first
-- option found is used.
--
-- For now we only parse command line arguments.
loadConfig :: IO CmdLineConfig
loadConfig = parseCmdLine

-- | File name to look for.
-- configFileName :: String
-- configFileName = "txs.yaml"

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
-- loadConfigFromFile :: IO UnintConfig
-- loadConfigFromFile = undefined

