{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric #-}
-- | Configuration related functionality for TorXakis server.
module TxsServerConfig
  ( loadConfig
  , interpretConfig
  , Config (..)
  , UnintConfig(..)
  , loadConfigFromFile -- TODO: hide this.
  )
where

import           CmdLineParser
import           Config
import           Data.Aeson.Types
import           Data.Foldable
import           Data.Monoid
import           Data.Yaml
import           GHC.Generics
import           Network
import           System.Directory
import           System.FilePath

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
configFileName :: FilePath
configFileName = ".torxakis.yaml"

-- | Structure of the configuration file.
data FileConfig = FileConfig
  { -- | Log all SMT commands?
    fcSmtLog           :: Maybe Bool
    -- | Available solvers that can be chosen from.
  , fcAvailableSolvers :: Maybe [SolverFileConfig]
  , fcSelectedSolver   :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON FileConfig where
    parseJSON = genericParseJSON
      defaultOptions
      { fieldLabelModifier = fieldsMapping
      , omitNothingFields  = True
      }
      where
        fieldsMapping "fcSelectedSolver"   = "selected-solver"
        fieldsMapping "fcAvailableSolvers" = "available-solvers"
        fieldsMapping "fcSmtLog"           = "smt-log"
        fieldsMapping x                    = x

data SolverFileConfig = SolverFileConfig
  { fcSolverId       :: String
  , fcExecutableName :: FilePath
  , fcFlags          :: Maybe [String]
  } deriving (Eq, Show, Generic)

instance FromJSON SolverFileConfig where
  parseJSON = genericParseJSON
    defaultOptions
    { fieldLabelModifier = fieldsMapping
    , omitNothingFields  = True
    }
    where fieldsMapping "fcSolverId"       = "solver-id"
          fieldsMapping "fcExecutableName" = "executable-name"
          fieldsMapping "fcFlags"          = "flags"
          fieldsMapping x                  = x

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
loadConfigFromFile :: IO (Either ParseException FileConfig)
loadConfigFromFile = do
  -- Check for the configuration file in the current directory.
  fileInCurrentDir <- doesFileExist configFileName
  if fileInCurrentDir
    then decodeFileEither configFileName
    else do
    home <- getHomeDirectory
    let path = home </> configFileName
    fileInHomeDir <- doesFileExist path
    if fileInHomeDir
      then decodeFileEither path
      else return (Left $ AesonException path) -- TODO: this should be another exception (like file not found).



