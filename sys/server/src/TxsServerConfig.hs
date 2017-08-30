{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- | Configuration related functionality for TorXakis server.
module TxsServerConfig (loadConfig, interpretConfig, Config (..), UnintConfig(..)) where

import           CmdLineParser
import           Config
import           Data.Foldable
import           Data.Monoid
import           Network

-- | Uninterpreted configuration options.
type UnintConfig = CmdLineConfig -- TODO: termporary alias
-- data UnintConfig = UnintConfig
--   { mSmtSolver  :: !(Maybe CoreConfig.SMTSolver)
--   , mSmtLog     :: !(Maybe Bool)
--   , mPortNumber :: !(Maybe PortNumber)
--   }

type Error = String

interpretConfig :: UnintConfig -> Either [Error] Config
interpretConfig uCfg = Right $ cfgMod defaultConfig
  where
    cfgMod :: Config -> Config
    cfgMod = appEndo $ foldMap Endo cfgMods
    cfgMods :: [Config -> Config]
    cfgMods = [changeSolver, changeSmtLog]
    changeSolver :: Config -> Config
    changeSolver cfg =
      case clSmtSolver uCfg of
        Nothing ->
          cfg
        Just solver ->
          cfg { selectedSolver = SolverId solver }
    changeSmtLog cfg =
      case clSmtLog uCfg of -- TODO: refactor this duplication
        Nothing ->
          cfg
        Just val ->
          cfg { smtLog = val }


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
-- configFileName :: FilePath
-- configFileName = ".torxakis.yml"

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

