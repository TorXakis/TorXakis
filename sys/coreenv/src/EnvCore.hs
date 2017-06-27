{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module EnvCore

-- ----------------------------------------------------------------------------------------- --
--
-- TorXakis Core Environment (Internal State) Data Type Definitions
--
-- ----------------------------------------------------------------------------------------- --
-- export

( IOC             -- IOC = StateT EnvC IO
                  -- torxakis core main state monad transformer
, EnvC (..)       -- torxakis core state
, getSMT          -- :: String -> IOC SMTData.SmtEnv
, putSMT          -- :: String -> SMTData.SmtEnv -> IOC ()
, getParams       -- :: [String] -> IOC [(String,String)]
, setParams       -- :: [(String,String)] -> IOC [(String,String)]
, initUnid        -- :: IOC.IOC Int
, newUnid         -- :: IOC.IOC Int
, putMsgs         -- :: [EnvData.Msg] -> IOC ()
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Control.Monad.State

import qualified Data.Map  as Map

-- import from local
import qualified EnvData   as EnvData
import qualified ParamCore as ParamCore

-- import from behavedefs
import qualified BTree     as BTree

-- import from defs
import qualified TxsDefs   as TxsDefs
import qualified TxsDDefs  as TxsDDefs
import qualified TxsShow   as TxsShow

-- import from solve
import qualified SMTData   as SMTData
 

-- ----------------------------------------------------------------------------------------- --
-- IOC :  torxakis core state monad transformer


type  IOC   =  StateT EnvC IO


data  EnvC  =  Noning   { params    :: ParamCore.Params              -- parameters
                        , unid      :: Int                           -- last used unique number
                        }
             | Initing  { smts      :: Map.Map String SMTData.SmtEnv -- named smt solver envs
                        , tdefs     :: TxsDefs.TxsDefs               -- TorXakis definitions
                        , params    :: ParamCore.Params              -- parameters
                        , unid      :: Int                           -- last used unique number
                        , putmsgs   :: [EnvData.Msg] -> IOC ()       -- (error) reporting
                        } 
             | Testing  { smts      :: Map.Map String SMTData.SmtEnv -- named smt solver envs
                        , tdefs     :: TxsDefs.TxsDefs               -- TorXakis definitions
                        , modeldef  :: TxsDefs.TxsDef
                        , mapperdef :: TxsDefs.TxsDef
                        , purpdef   :: TxsDefs.TxsDef
                        , puttow    :: TxsDDefs.Action -> IOC TxsDDefs.Action
                        , getfrow   :: IOC TxsDDefs.Action
                        , behtrie   :: [ (EnvData.StateNr, TxsDDefs.Action, EnvData.StateNr) ]
                                                                     -- behaviour trie
                        , inistate  :: EnvData.StateNr               -- initial beh statenr
                        , curstate  :: EnvData.StateNr               -- current beh statenr
                        , modsts    :: BTree.BTree                      -- model state
                        , mapsts    :: BTree.BTree                      -- mapper state
                        , purpsts   :: [(TxsDefs.GoalId,BTree.BTree)]   -- purpose state
                        , params    :: ParamCore.Params              -- parameters
                        , unid      :: Int                           -- last used unique number
                        , putmsgs   :: [EnvData.Msg] -> IOC ()       -- (error) reporting
                        } 
             | Simuling { smts      :: Map.Map String SMTData.SmtEnv -- named smt solver envs
                        , tdefs     :: TxsDefs.TxsDefs               -- TorXakis definitions
                        , modeldef  :: TxsDefs.TxsDef
                        , mapperdef :: TxsDefs.TxsDef
                        , puttow    :: TxsDDefs.Action -> IOC TxsDDefs.Action
                        , getfrow   :: IOC TxsDDefs.Action
                        , behtrie   :: [(EnvData.StateNr,TxsDDefs.Action,EnvData.StateNr)]
                                                                     -- behaviour trie
                        , inistate  :: EnvData.StateNr               -- initial beh statenr
                        , curstate  :: EnvData.StateNr               -- current beh statenr
                        , modsts    :: BTree.BTree                   -- model state
                        , mapsts    :: BTree.BTree                   -- mapper state
                        , params    :: ParamCore.Params              -- parameters
                        , unid      :: Int                           -- last used unique number
                        , putmsgs   :: [EnvData.Msg] -> IOC ()       -- (error) reporting
                        } 
             | Stepping { smts      :: Map.Map String SMTData.SmtEnv -- named smt solver envs
                        , tdefs     :: TxsDefs.TxsDefs               -- TorXakis definitions
                        , modeldef  :: TxsDefs.TxsDef
                        , behtrie   :: [(EnvData.StateNr,TxsDDefs.Action,EnvData.StateNr)]
                                                                     -- behaviour trie
                        , inistate  :: EnvData.StateNr               -- initial beh statenr
                        , curstate  :: EnvData.StateNr               -- current beh statenr
                        , maxstate  :: EnvData.StateNr               -- max beh statenr
                        , modstss   :: Map.Map EnvData.StateNr BTree.BTree   -- model state
                        , params    :: ParamCore.Params              -- parameters
                        , unid      :: Int                           -- last used unique number
                        , putmsgs   :: [EnvData.Msg] -> IOC ()       -- (error) reporting
                        } 


-- ----------------------------------------------------------------------------------------- --
-- SMT :  getting and setting SMT solver


getSMT :: String -> IOC SMTData.SmtEnv
getSMT smtname  =  do
     smts    <- gets smts
     putMsgs <- gets putmsgs
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
                                return $ SMTData.SmtEnvError
       Just smtenv -> return smtenv


putSMT :: String -> SMTData.SmtEnv -> IOC ()
putSMT smtname smtenv  =  do
     smts' <- gets smts
     modify $ \env -> env { smts = Map.insert smtname smtenv smts' }
 

-- ----------------------------------------------------------------------------------------- --
-- Params :  getParams, setParams 


getParams :: [String] -> IOC [(String,String)]
getParams prms  =  do
     case prms of
       [] -> do parammap <- gets params
                return $ map (\(nm,(val,_))->(nm,val)) (Map.toList parammap)
       _  -> do params <- mapM getParam prms
                return $ concat params

getParam :: String -> IOC [(String,String)]
getParam prm  =  do
     params <- gets params
     case Map.lookup prm params of
       Nothing          -> return []
       Just (val,check) -> return [(prm,val)]


setParams :: [(String,String)] -> IOC [(String,String)]
setParams parvals  =  do
     params <- mapM setParam parvals
     return $ concat params

setParam :: (String,String) -> IOC [(String,String)]
setParam (prm,val)  =  do
     params <- gets params
     case Map.lookup prm params of
       Nothing           -> do return []
       Just (val',check) -> if  check val
                              then do params' <- return $ Map.insert prm (val,check) params
                                      modify $ \env -> env { params = params' }
                                      return [(prm,val)]
                              else do return []


-- ----------------------------------------------------------------------------------------- --
-- Unid :  unique (negative) number for identifiers


initUnid :: IOC Int
initUnid  =  do
     return (-1)


newUnid :: IOC Int
newUnid  =  do
     uid <- gets unid
     modify $ \env -> env { unid = uid - 1 }
     return $ uid - 1


-- ----------------------------------------------------------------------------------------- --
-- put messages


putMsgs :: [EnvData.Msg] -> IOC ()
putMsgs msg  =  do
     putMsgs' <- gets putmsgs
     putMsgs' msg


-- ----------------------------------------------------------------------------------------- --
-- 
-- ----------------------------------------------------------------------------------------- --

