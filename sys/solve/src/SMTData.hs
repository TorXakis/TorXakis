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
)
where

import           Control.Monad.State
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Data.Text           (Text)
import           System.IO
import           System.Process

import           FuncId
import           Identifier
import           Sort

-- ----------------------------------------------------------------------------------------- --
-- SMT state monad for smt solver

data  SmtEnv  =  SmtEnv { inHandle          :: Handle
                        , outHandle         :: Handle
                        , errHandle         :: Handle
                        , smtProcessHandle  :: ProcessHandle
                        , logFileHandle     :: Maybe Handle
                        , adtDefs           :: ADTDefs
                        , funcIds           :: Set.Set FuncId
                        , decoderMap        :: Map.Map Text (Ref ADTDef, Ref ConstructorDef)
                        }
               | SmtEnvError

instance Show SmtEnv where
  show smtEnv =  show $ decoderMap smtEnv

type  SMT a   =  StateT SmtEnv IO a
