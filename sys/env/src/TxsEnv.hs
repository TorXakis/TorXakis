{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module TxsEnv

-- ----------------------------------------------------------------------------------------- --
--
-- TorXakis Environment (Internal State) Data Type Definitions
--
-- ----------------------------------------------------------------------------------------- --

where

import System.IO
import Network
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Time
import Data.String.Utils

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import TxsDDefs
import TxsDefs
import TxsShow
import Params
import SolveDefs.Params

import CTree
import SMTData(SmtEnv)


-- ----------------------------------------------------------------------------------------- --
-- IOE :  main monad state transformer


type  IOE a     =  StateT Env IO a


liftP2 :: Monad m => (a, m b) -> m (a, b)
liftP2 (x, my)  =  do  { y <- my; return (x,y) }

liftP3 :: Monad m => (a, b, m c) -> m (a, b, c)
liftP3 (x, y, mz)  =  do  { z <- mz; return (x,y,z) }


data  TxsModus  =  Noning
                 | Idling 
                 | Initing
                 | Testing  ModelId CnectId
                 | Simuling ModelId CnectId
                 | Stepping ModelId
     deriving (Eq,Ord,Read,Show)


isNoning (Noning)          =  True
isNoning  _                =  False
isIdling (Idling)          =  True
isIdling  _                =  False
isIniting (Initing)        =  True
isIniting  _               =  False
isTesting (Testing _ _)    =  True
isTesting  _               =  False
isSimuling (Simuling _ _)  =  True
isSimuling  _              =  False 
isStepping (Stepping _)    =  True
isStepping  _              =  False

isNotNoning (Noning)       =  False
isNotNoning  _             =  True
isNotIdling (Idling)       =  False
isNotIdling  m             =  isNotNoning m
isNotIniting (Initing)     =  False
isNotIniting  m            =  isNotIdling m

-- --------------------------------------------------------------------------------------- --
-- Env :  main state environment Env


data  Env  =  Env { envhost   :: String
                  , envportnr :: Integer
                  , envservhs :: Handle                    -- server socket handle
                  , envmodus  :: TxsModus                  -- current modus of TXS operation
                  , envuid    :: Int                       -- last used unique id number
                  , envtdefs  :: TxsDefs                   -- TorXakis definitions from file
                  , envsmts   :: Map.Map String SmtEnv     -- named smt solver handles
                  , envinit   :: StatNr                    -- initial state nr
                  , envnrst   :: StatNr                    -- number of states
                  , envcurs   :: StatNr                    -- current trie state nr
                  , envtrie   :: [(StatNr,Action,StatNr)]  -- transitions in prefix tree
                  , envs2bt   :: Map.Map StatNr BTree      -- map trie state to btree
                  , envpurp   :: [(GoalId,BTree)]          -- current test purpose
                  , envmapper :: BTree                     -- current btree of mapper
                  , envtow    :: ( Maybe (Chan SAction), Maybe ThreadId, [ConnHandle] )
                  , envfrow   :: ( Maybe (Chan SAction), [ThreadId],     [ConnHandle] )
                                                           -- connections to/from world
                  , envvar    :: [VarId]                   -- local free variables
                  , envval    :: VEnv                      -- local value environment
                  , envparams :: Map.Map String ( String, String -> Bool )
                                                           -- TorXakis parameters with checks
                  }


envNone :: Env
envNone  =  Env  { envhost   = ""
                 , envportnr = 0
                 , envservhs = stderr
                 , envmodus  = Noning
                 , envuid    = 1000
                 , envtdefs  = TxsDefs.empty
                 , envsmts   = Map.empty
                 , envinit   = (-1)
                 , envnrst   = 0
                 , envcurs   = (-1)
                 , envtrie   = []
                 , envs2bt   = Map.empty
                 , envpurp   = []
                 , envmapper = []
                 , envtow    = ( Nothing, Nothing, [] )
                 , envfrow   = ( Nothing, [],      [] )
                 , envvar    = []
                 , envval    = Map.empty
                 , envparams = Map.union Params.initParams SolveDefs.Params.initParams
                 }


envIdle :: Env
envIdle  =  envNone { envmodus = Idling }


envInit :: Env
envInit =  envIdle { envmodus = Initing }


-- ----------------------------------------------------------------------------------------- --
-- StatNr :  trie (prefix tree) state number


type  StatNr   = Int


-- ----------------------------------------------------------------------------------------- --
-- ----------------------------------------------------------------------------------------- --
-- getSpec :  get current ModelDef from Env

getSpec :: IOE TxsDef
getSpec  =  do
     modus  <- gets envmodus
     tdefs  <- gets envtdefs
     specid <- return $ case modus of
                        { Idling                         -> ModelId "" 0
                        ; Testing  modid cnectid         -> modid
                        ; Simuling modid cnectid         -> modid
                        ; Stepping modid                 -> modid 
                        } 
     case Map.lookup specid (modelDefs tdefs) of
     { Just specdef -> return $ DefModel specdef
     ; otherwise    -> do lift $ hPutStrLn stderr $ "TXS TxsEnv getSpec: No current Spec\n"
                          return $ DefNo
     }


{-

getMapper :: IOE TxsDef
getMapper  =  do
     modus  <- gets envmodus
     tdefs  <- gets envtdefs
     mapid <- return $ case modus of
                       { Idling                         -> ModelId "" 0
                       ; Testing modid mapid cnectid    -> mapid
                       ; Simuling modid mapid cnectid -> mapid
                       ; Stepping modid                 -> ModelId "" 0
                       }
     case Map.lookup mapid tdefs of
     { Just moddef@(ModelDef _ _ _) -> do return $ moddef
     ; otherwise                    -> do lift $ hPutStrLn stderr
                                               $ "TXS TxsEnv getMapper: No current Mapper\n"
                                          return $ NoDef
     }

-}


-- ----------------------------------------------------------------------------------------- --
-- getBTree :  get current BTree from Env

getBTree :: IOE BTree
getBTree  =  do
     curstate    <- gets envcurs
     state2btree <- gets envs2bt
     case Map.lookup curstate state2btree of
     { Just btree -> do return $ btree
     ; otherwise  -> do lift $ hPutStrLn stderr
                             $ "TXS getBTree: No current State\n"
                        return $ []
     }


-- ----------------------------------------------------------------------------------------- --
-- get/setParam :  get/set parameter from Env

getParam :: String -> IOE String
getParam param  =  do
     params <- gets envparams
     case Map.lookup param params of
     { Nothing    -> do lift $ hPutStrLn stderr
                             $ "TXS getParam: No such parameter: "++param++"\n"
                        return $ param
     ; Just (s,c) -> do return $ s
     }

setParam :: String -> String -> IOE ()
setParam param val  =  do
     params  <- gets envparams
     case Map.lookup param params of
     { Nothing    -> do lift $ hPutStrLn stderr
                             $ "TXS getParam: No such parameter: "++param++"\n"
     ; Just (s,c) -> if  c val
                       then do params' <- return $ Map.insert param (val,c) params
                               modify ( \env -> env { envparams = params' } )
                       else do lift $ hPutStrLn stderr
                                    $ "TXS getParam: Incorrect parameter value\n"
     }


-- ----------------------------------------------------------------------------------------- --
-- get SMT solver

getSMT :: String -> IOE SmtEnv
getSMT smtname  =  do
     smts <- gets envsmts
     case Map.lookup smtname smts of
     { Nothing  -> do lift $ hPutStrLn stderr
                           $ "TXS getSMT: No such SMT solve: "++smtname++"\n"
                      return $ if  not $ Map.null smts
                                 then snd $ head $ Map.toList smts
                                 else error $ "TXS getSMT: No SMT solver at all\n"
     ; Just smt -> do return $ smt
     }

-- ----------------------------------------------------------------------------------------- --
-- put SMT solver

putSMT :: String -> SmtEnv -> IOE ()
putSMT smtname smtenv =  do
     smts <- gets envsmts
     modify (\env -> env { envsmts = Map.insert smtname smtenv smts })

-- ----------------------------------------------------------------------------------------- --
-- conditional output: smt debug


putSmtDebug :: String -> IOE ()
putSmtDebug s  =  do
     param_SMT_debug <- getParam "param_SMT_debug"
     when (read param_SMT_debug) $ lift $ hPutStrLn stderr $ "SMT >> " ++ s

hPutSmtLog :: Handle -> String -> IOE ()
hPutSmtLog log s  =  do
     when (log /= stderr) $ lift $ hPutStrLn log s
                                       

-- ----------------------------------------------------------------------------------------- --
-- 
-- ----------------------------------------------------------------------------------------- --

