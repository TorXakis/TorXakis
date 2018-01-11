{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SMTData
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structure for SMT Data.
-----------------------------------------------------------------------------

module SMTData
( SMT
, SmtEnv(..)
, EnvNames(..)
, EnvDefs (..)
)
where

import           Control.Monad.State
import           Data.Text           (Text)

import           System.IO
import           System.Process

import           ADTDef
import qualified Data.Map            as Map
import           FuncDef
import           FuncId
import           Identifier
import           SortDef
import           VarId

-- ----------------------------------------------------------------------------------------- --
-- SMT state monad for smt solver

data EnvDefs = EnvDefs { adtDefs    :: ADTDefs
                       , funcDefs   :: Map.Map FuncId (FuncDef VarId)
                       }
               deriving (Eq,Ord,Read,Show)

data EnvNames = EnvNames { sortNames   :: Map.Map (TRef SortDef) Text
                         , funcNames   :: Map.Map FuncId         Text
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
