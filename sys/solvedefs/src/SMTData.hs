{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
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
, getParam
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
                            , params                    :: Map.Map String ( String, String -> Bool )
                            }

type  SMT a   =  StateT SmtEnv IO a

instance Show SmtEnv
   where
       show smtEnv =  show $ mapInstanceTxsToSmtlib smtEnv

-- ------------------------------------------------------------------------------
-- convenience function
-- ------------------------------------------------------------------------------
getParam :: String -> StateT SmtEnv IO String
getParam p  =  do
     ps <- gets params
     case Map.lookup p ps of
       Nothing    -> error $ "SMT getParam: No such parameter: " ++ p
       Just (s,_) -> do return $ s
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

