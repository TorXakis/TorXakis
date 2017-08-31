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
import           Control.Monad.Extra
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Foldable
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Yaml
import           GHC.Generics
import           Network
import           System.Directory
import           System.FilePath

-- | Uninterpreted configuration options.
data UnintConfig = UnintConfig
  { -- | Configuration options passed via the command line.
    cmdLineCfg :: CmdLineConfig
    -- | Configuration options specified in a configuration file. The
    -- configuration file is optional, hence the use of `Maybe`.
  , fileCfg    :: Maybe FileConfig
  } deriving (Show, Eq)

type Error = String

interpretConfig :: UnintConfig -> Either [Error] Config
interpretConfig uCfg =
  let cfg = cfgMod defaultConfig in
    if Map.member (selectedSolver cfg) (availableSolvers cfg)
    then Right cfg
    else Left [ "No solver found with id " ++ show (selectedSolver cfg)
              ++ " in the available solvers: " ++ show (availableSolvers cfg)
              ]
  where
    cfgMod :: Config -> Config
    cfgMod = appEndo $ foldMap Endo (clCfgMods ++ fcCfgMods)
    clCfgMods = [clChangeSolver, clChangeSmtLog, fcChangeAvailableSolvers]
    fcCfgMods = [fcChangeSolver, fcChangeSmtLog]
    clChangeSolver =
      updateCfg ((clSmtSolver . cmdLineCfg) uCfg) changeSolver
    clChangeSmtLog  =
      updateCfg ((clSmtLog . cmdLineCfg) uCfg) changeLog
    fcChangeSolver =
      updateCfg (fileCfg uCfg >>= fcSelectedSolver) changeSolver
    fcChangeSmtLog =
      updateCfg (fileCfg uCfg >>= fcSmtLog) changeLog
    fcChangeAvailableSolvers =
      updateCfg (Map.fromList . map toKV <$>
                  (fileCfg uCfg >>= fcAvailableSolvers)
                )
                addSolvers
    toKV solverFC =
      ( SolverId (fcSolverId solverFC)
      , SolverConfig (fcExecutableName solverFC)
                     (fromMaybe [] . fcFlags $ solverFC)
      )

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
loadConfig :: IO UnintConfig
loadConfig = UnintConfig <$> parseCmdLine <*> loadConfigFromFile

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
loadConfigFromFile :: IO (Maybe FileConfig)
loadConfigFromFile = do
  mPath <- findConfigFile
  case mPath of
    Nothing ->
      return Nothing
    Just path -> do
      res <- decodeFileEither path
      case res of
        Left err   -> error (show err)
        Right fCfg -> return (Just fCfg)

-- | Tries to find the TorXakis configuration file. Returns the first
--  configuration file found, or nothing otherwise
findConfigFile :: IO (Maybe FilePath)
findConfigFile = do
  home <- getHomeDirectory
  findM doesFileExist [configFileName, home </> configFileName]


