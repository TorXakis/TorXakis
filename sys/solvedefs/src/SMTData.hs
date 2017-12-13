{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module SMTData

-- ----------------------------------------------------------------------------------------- --
--
-- SMT Data type
--
-- ----------------------------------------------------------------------------------------- --
-- export

( SMT
, SmtEnv(..)
, EnvNames(..)
, EnvDefs (..)
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State
import           Data.Text           (Text)

import           System.IO
import           System.Process

import qualified Data.Map            as Map
import           CstrDef
import           CstrId
import           FuncDef
import           FuncId
import           SortDef
import           SortId
import           VarId

-- ----------------------------------------------------------------------------------------- --
-- SMT state monad for smt solver

data EnvDefs = EnvDefs { sortDefs   :: Map.Map SortId SortDef
                       , cstrDefs   :: Map.Map CstrId CstrDef
                       , funcDefs   :: Map.Map FuncId (FuncDef VarId)
                       }
               deriving (Eq,Ord,Read,Show)

data EnvNames = EnvNames { sortNames   :: Map.Map SortId Text
                         , cstrNames   :: Map.Map CstrId Text
                         , funcNames   :: Map.Map FuncId Text
                         }
                deriving (Eq,Ord,Read,Show)

data  SmtEnv  =  SmtEnv     { inHandle          :: Handle
                            , outHandle         :: Handle
                            , errHandle         :: Handle
                            , smtProcessHandle  :: ProcessHandle
                            , logFileHandle     :: Maybe Handle
                            , envNames          :: EnvNames
                            , envDefs           :: EnvDefs
                            }
               | SmtEnvError

type  SMT a   =  StateT SmtEnv IO a

instance Show SmtEnv where
  show smtEnv =  show $ envNames smtEnv

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
