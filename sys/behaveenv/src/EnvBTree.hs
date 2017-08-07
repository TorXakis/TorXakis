{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module EnvBTree

-- ----------------------------------------------------------------------------------------- --
--
-- Interaction Variables, Communication Tree, Behaviour Tree, Behaviour Node
--
-- ----------------------------------------------------------------------------------------- --
-- export

( IOB
, EnvB (..)
, getSMT          -- getSMT :: String -> IOB SMTData.SmtEnv
, putSMT          -- putSMT :: String -> SMTData.SmtEnv -> IOB ()
, putMsgs         -- putSMT :: String -> SMTData.SmtEnv -> IOB ()
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Control.Monad.State

-- import qualified Data.Char as Char
-- import qualified Data.List as List
-- import qualified Data.Set  as Set
import qualified Data.Map  as Map
-- import qualified Data.String.Utils as Utils

-- import from coreenv
-- import qualified EnvCore  as IOC
import qualified EnvData
import qualified ParamCore

-- import from defs
import qualified TxsDefs
import qualified Sigs
import VarId

-- import from solve
import qualified SMTData


-- ----------------------------------------------------------------------------------------- --
-- IOB :  behaviour environment state monad transformer
--        projection of IOC that are necessary in BTree manipulation


type  IOB a  =  StateT EnvB IO a


data  EnvB   =  EnvB { smts     :: Map.Map String SMTData.SmtEnv -- named smt solver envs
                     , tdefs    :: TxsDefs.TxsDefs               -- TorXakis definitions
                     , sigs     :: Sigs.Sigs VarId
                     , stateid  :: EnvData.StateNr               -- current beh statenr
                     , params   :: ParamCore.Params              -- parameters
                     , unid     :: Int                           -- last used unique number
                     , msgs     :: [EnvData.Msg]                 -- (error) reporting
                     }


-- ----------------------------------------------------------------------------------------- --
-- SMT :  getting and setting SMT solver


getSMT :: String -> IOB SMTData.SmtEnv
getSMT smtname  =  do
     smts    <- gets smts
     case Map.lookup smtname smts of
       Nothing     -> if  not $ Map.null smts
                        then do putMsgs [ EnvData.TXS_CORE_SYSTEM_WARNING 
                                          $ "No such Solver: " ++ smtname ]
                                (name,smtenv) <- return $ head $ Map.toList smts
                                putMsgs [ EnvData.TXS_CORE_SYSTEM_WARNING 
                                          $ "Using instead: " ++ name ] 
                                return smtenv
                        else do putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR 
                                          $ "No such Solver: " ++ smtname ]
                                return SMTData.SmtEnvError
       Just smtenv -> return smtenv


putSMT :: String -> SMTData.SmtEnv -> IOB ()
putSMT smtname smtenv  =  do
     smts' <- gets smts
     modify $ \env -> env { smts = Map.insert smtname smtenv smts' }


-- ----------------------------------------------------------------------------------------- --
-- Add messages to IOB Monad.


-- | Add messages to IOB Monad.
putMsgs :: [EnvData.Msg] -> IOB () 
putMsgs mess  =  do
     msgs' <- gets msgs
     modify $ \env -> env { msgs = msgs' ++ mess }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
