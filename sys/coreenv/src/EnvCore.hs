{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


{-# LANGUAGE FlexibleInstances #-}
-- | TorXakis Core Environment (Internal State) Data Type Definitions.
module EnvCore
  ( IOC -- IOC = StateT EnvC IO
                  -- torxakis core main state monad transformer
  , EnvC(..)
  , CoreState(..)
  , getSMT -- :: String -> IOC SMTData.SmtEnv
  , putSMT -- :: String -> SMTData.SmtEnv -> IOC ()
  , getParams -- :: [String] -> IOC [(String,String)]
  , setParams -- :: [(String,String)] -> IOC [(String,String)]
  , initUnid -- :: IOC.IOC Int
  , newUnid -- :: IOC.IOC Int
  , putMsgs -- :: [EnvData.Msg] -> IOC ()
  -- * Operation on core-state
  , modifyCS
  , putCS
  , incUnid
  )
where

import           Control.Monad.State hiding (state)

import qualified Data.Map            as Map


-- import from local
import           Config    hiding (setParams)
import qualified EnvBasic    as EnvB
import qualified EnvData
import qualified ParamCore

-- import from behavedefs
import qualified BTree

-- import from defs
import qualified Sigs
import qualified TxsDDefs
import qualified TxsDefs

-- import from valexpr
import           Id
import qualified VarId               (VarId)

-- import from solve
import qualified SMTData

-- ----------------------------------------------------------------------------------------- --
-- IOC :  torxakis core state monad transformer

type  IOC  = StateT EnvC IO

instance EnvB.EnvB IOC     --  (StateT IOC.EnvC IO)
  where
     newUnid  =  newUnid
     putMsgs  =  putMsgs
 

data EnvC = EnvC
  { config :: Config           -- ^ Core configuration.
  , unid   :: Id               -- ^ Last used unique number.
  , params :: ParamCore.Params
  , state  :: CoreState        -- ^ State specific information.
  }

data CoreState = Noning
             | Initing  { smts    :: Map.Map String SMTData.SmtEnv -- named smt solver envs
                        , tdefs   :: TxsDefs.TxsDefs               -- TorXakis definitions
                        , sigs    :: Sigs.Sigs VarId.VarId       -- TorXakis signatures
                        , putmsgs :: [EnvData.Msg] -> IOC ()       -- (error) reporting
                        }
             | Testing  { smts      :: Map.Map String SMTData.SmtEnv -- named smt solver envs
                        , tdefs     :: TxsDefs.TxsDefs               -- TorXakis definitions
                        , sigs      :: Sigs.Sigs VarId.VarId       -- TorXakis signatures
                        , modeldef  :: TxsDefs.ModelDef
                        , mapperdef :: Maybe TxsDefs.MapperDef
                        , purpdef   :: Maybe TxsDefs.PurpDef
                        , puttow    :: TxsDDefs.Action -> IOC TxsDDefs.Action
                        , getfrow   :: IOC TxsDDefs.Action
                        , behtrie   :: [ (EnvData.StateNr, TxsDDefs.Action, EnvData.StateNr) ]
                                                                     -- behaviour trie
                        , inistate  :: EnvData.StateNr               -- initial beh statenr
                        , curstate  :: EnvData.StateNr               -- current beh statenr
                        , modsts    :: BTree.BTree                      -- model state
                        , mapsts    :: BTree.BTree                      -- mapper state
                        , purpsts   :: [(TxsDefs.GoalId,Either BTree.BTree TxsDDefs.PurpVerdict)]   -- purpose state
                        , putmsgs   :: [EnvData.Msg] -> IOC ()       -- (error) reporting
                        }
             | Simuling { smts      :: Map.Map String SMTData.SmtEnv -- named smt solver envs
                        , tdefs     :: TxsDefs.TxsDefs               -- TorXakis definitions
                        , sigs      :: Sigs.Sigs VarId.VarId       -- TorXakis signatures
                        , modeldef  :: TxsDefs.ModelDef
                        , mapperdef :: Maybe TxsDefs.MapperDef
                        , puttow    :: TxsDDefs.Action -> IOC TxsDDefs.Action
                        , getfrow   :: IOC TxsDDefs.Action
                        , behtrie   :: [(EnvData.StateNr,TxsDDefs.Action,EnvData.StateNr)]
                                                                     -- behaviour trie
                        , inistate  :: EnvData.StateNr               -- initial beh statenr
                        , curstate  :: EnvData.StateNr               -- current beh statenr
                        , modsts    :: BTree.BTree                   -- model state
                        , mapsts    :: BTree.BTree                   -- mapper state
                        , putmsgs   :: [EnvData.Msg] -> IOC ()       -- (error) reporting
                        }
             | Stepping { smts      :: Map.Map String SMTData.SmtEnv -- named smt solver envs
                        , tdefs     :: TxsDefs.TxsDefs               -- TorXakis definitions
                        , sigs      :: Sigs.Sigs VarId.VarId       -- TorXakis signatures
                        , modeldef  :: TxsDefs.ModelDef
                        , behtrie   :: [(EnvData.StateNr,TxsDDefs.Action,EnvData.StateNr)]
                                                                     -- behaviour trie
                        , inistate  :: EnvData.StateNr               -- initial beh statenr
                        , curstate  :: EnvData.StateNr               -- current beh statenr
                        , maxstate  :: EnvData.StateNr               -- max beh statenr
                        , modstss   :: Map.Map EnvData.StateNr BTree.BTree   -- model state
                        , putmsgs   :: [EnvData.Msg] -> IOC ()       -- (error) reporting
                        }


modifyCS :: (CoreState -> CoreState) -> IOC ()
modifyCS f  = modify $ \env -> env { state = f (state env) }

putCS :: CoreState -> IOC ()
putCS newSt = modify $ \env -> env { state = newSt }

incUnid :: IOC ()
incUnid = modify $ \env -> env { unid = unid env + 1}

-- ----------------------------------------------------------------------------------------- --
-- SMT :  getting and setting SMT solver

getSMT :: String -> IOC SMTData.SmtEnv
getSMT smtname = do
     smts'    <- gets (smts . state)
     putMsgs' <- gets (putmsgs . state)
     case Map.lookup smtname smts' of
       Nothing     -> if  not $ Map.null smts'
                        then do putMsgs' [ EnvData.TXS_CORE_SYSTEM_WARNING
                                          $ "No such Solver: " ++ smtname ]
                                let (name',smtenv) = head $ Map.toList smts'
                                putMsgs' [ EnvData.TXS_CORE_SYSTEM_WARNING
                                          $ "Using instead: " ++ name' ]
                                return smtenv
                        else do putMsgs' [ EnvData.TXS_CORE_SYSTEM_ERROR
                                          $ "No such Solver: " ++ smtname ]
                                return SMTData.SmtEnvError
       Just smtenv -> return smtenv

putSMT :: String -> SMTData.SmtEnv -> IOC ()
putSMT smtname smtenv = do
  st <- gets state
  let smts' = smts st
      state' = st { smts = Map.insert smtname smtenv smts'}
  modify $ \env -> env { state = state' }

-- ----------------------------------------------------------------------------------------- --
-- Params :  getParams, setParams

getParams :: [String] -> IOC [(String,String)]
getParams prms =
     case prms of
       [] -> do parammap <- gets params
                return $ map (\(nm,(val,_))->(nm,val)) (Map.toList parammap)
       _  -> do params' <- mapM getParam prms
                return $ concat params'

getParam :: String -> IOC [(String,String)]
getParam prm = do
     params' <- gets params
     case Map.lookup prm params' of
       Nothing           -> return []
       Just (val,_check) -> return [(prm,val)]

setParams :: [(String,String)] -> IOC [(String,String)]
setParams parvals = do
     params' <- mapM setParam parvals
     return $ concat params'

setParam :: (String,String) -> IOC [(String,String)]
setParam (prm,val) = do
     params' <- gets params
     case Map.lookup prm params' of
       Nothing           -> return []
       Just (_,check)    -> if check val
                              then let newParams = Map.insert prm (val,check) params'
                                    in do
                                      modify $ \env -> env { params = newParams }
                                      return [(prm,val)]
                              else return []

-- ----------------------------------------------------------------------------------------- --
-- Unid :  unique (negative) number for identifiers

initUnid :: IOC Id
initUnid = return (-1)

newUnid :: IOC Id
newUnid = do
     uid <- gets unid
     modify $ \env -> env { unid = uid - 1 }
     return $ uid - 1

-- ----------------------------------------------------------------------------------------- --
-- put messages

putMsgs :: [EnvData.Msg] -> IOC ()
putMsgs msg = do
     putMsgs' <- gets (putmsgs . state)
     putMsgs' msg

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
