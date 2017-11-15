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
, putMsgs
, getFuncDefs
 -- * SMT querying and manipulation
, getSMT
, putSMT
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State
import qualified Data.Map            as Map
import qualified EnvData
import qualified ParamCore

-- import from defs
import qualified Sigs
import           TxsDefs

-- import from solve
import qualified SMTData

-- | IOB : behaviour environment state monad transformer projection of IOC that
-- are necessary in BTree manipulation
type IOB a = StateT EnvB IO a

data EnvB = EnvB { smts    :: Map.Map String SMTData.SmtEnv -- named smt solver envs
                 , tdefs   :: TxsDefs.TxsDefs               -- TorXakis definitions
                 , sigs    :: Sigs.Sigs VarId
                 , stateid :: EnvData.StateNr               -- current beh statenr
                 , params  :: ParamCore.Params              -- parameters
                 , unid    :: Int                           -- last used unique number
                 , msgs    :: [EnvData.Msg]                 -- (error) reporting
                 }

-- | Get the SMT solver.
getSMT :: String -> IOB SMTData.SmtEnv
getSMT smtname  =  do
     smts'    <- gets smts
     case Map.lookup smtname smts' of
       Nothing     -> if  not $ Map.null smts'
                        then do putMsgs [ EnvData.TXS_CORE_SYSTEM_WARNING
                                          $ "No such Solver: " ++ smtname ]
                                let (name',smtenv) = head $ Map.toList smts'
                                putMsgs [ EnvData.TXS_CORE_SYSTEM_WARNING
                                          $ "Using instead: " ++ name' ]
                                return smtenv
                        else do putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                          $ "No such Solver: " ++ smtname ]
                                return SMTData.SmtEnvError
       Just smtenv -> return smtenv

-- | Set the SMT solver.
putSMT :: String -> SMTData.SmtEnv -> IOB ()
putSMT smtname smtenv  =  do
     smts' <- gets smts
     modify $ \env -> env { smts = Map.insert smtname smtenv smts' }

-- | Add messages to IOB Monad.
putMsgs :: [EnvData.Msg] -> IOB ()
putMsgs mess  =  do
     msgs' <- gets msgs
     modify $ \env -> env { msgs = msgs' ++ mess }


-- | Get the function defined at in the current state.
getFuncDefs :: IOB (Map.Map FuncId (FuncDef VarId))
getFuncDefs = funcDefs . tdefs <$> get
