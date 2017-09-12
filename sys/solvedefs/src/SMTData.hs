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
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Control.Monad.State

import System.IO
import System.Process

import qualified Data.Map as Map

import TxsDefs


-- ----------------------------------------------------------------------------------------- --
-- SMT state monad for smt solver

data  SmtEnv  =  SmtEnv     { inHandle                  :: Handle
                            , outHandle                 :: Handle
                            , errHandle                 :: Handle
                            , smtProcessHandle          :: ProcessHandle
                            , logFileHandle             :: Maybe Handle
                            , mapInstanceTxsToSmtlib    :: Map.Map Ident String
                            , txsDefs                   :: TxsDefs
                            }
               | SmtEnvError

type  SMT a   =  StateT SmtEnv IO a

instance Show SmtEnv
   where
       show smtEnv =  show $ mapInstanceTxsToSmtlib smtEnv

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
