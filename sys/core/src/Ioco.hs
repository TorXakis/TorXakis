{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Ioco

-- ----------------------------------------------------------------------------------------- --
-- 
-- Test Primitives for an IOCO Model, built on Behave
-- 
-- ----------------------------------------------------------------------------------------- --
-- export


( iocoModelMenuIn    -- :: IOC.IOC BTree.Menu
, iocoModelMenuOut   -- :: IOC.IOC BTree.Menu
, iocoModelIsQui     -- :: IOC.IOC Bool
, iocoModelAfter     -- :: TxsDDefs.Action -> IOC.IOC Bool
)

-- ----------------------------------------------------------------------------------------- --
-- import 

where

import Control.Monad.State

import qualified Data.Set  as Set
import qualified Data.Map  as Map


-- import from local
import Trace
import CoreUtils

-- import from behavedef
import qualified BTree     as BTree

-- import from behaveenv
import qualified Behave    as Behave

-- import from coreenv
import qualified EnvCore   as IOC
import qualified EnvData   as EnvData

-- import from defs
import qualified TxsDefs   as  TxsDefs
import qualified TxsDDefs  as  TxsDDefs

import qualified Utils     as  Utils


-- ----------------------------------------------------------------------------------------- --
-- iocoModelMenuIn :  input menu on current btree of model, no quiescence, according to ioco


iocoModelMenuIn :: IOC.IOC BTree.Menu
iocoModelMenuIn  =  do
     menu <- traceModelMenu
     filterM (isInCTOffers . Utils.frst) menu


-- ----------------------------------------------------------------------------------------- --
-- iocoModelMenuOut :  output menu on current btree of model, no quiescence, according to ioco


iocoModelMenuOut :: IOC.IOC BTree.Menu
iocoModelMenuOut  =  do
     menu <- traceModelMenu
     filterM (isOutCTOffers . Utils.frst) menu


-- ----------------------------------------------------------------------------------------- --
-- iocoModelIsQui :  quiescence test on current btree of model 


iocoModelIsQui :: IOC.IOC Bool
iocoModelIsQui  =  do
     TxsDefs.DefModel (TxsDefs.ModelDef insyncs outsyncs splsyncs bexp) <- gets IOC.modeldef
     modSts   <- gets IOC.modsts
     return $ Behave.behRefusal modSts (Set.unions outsyncs)


-- ----------------------------------------------------------------------------------------- --
-- iocoModelAfter :  do action on current btree and change environment accordingly
-- result gives success, ie. whether act can be done, if not succesful env is not changed
-- act must be a non-empty action; Act ActIn ActOut or ActQui


iocoModelAfter :: TxsDDefs.Action -> IOC.IOC Bool

iocoModelAfter (TxsDDefs.Act acts)  =  do
     traceModelAfter acts

iocoModelAfter TxsDDefs.ActQui  =  do
     TxsDefs.DefModel (TxsDefs.ModelDef insyncs outsyncs splsyncs bexp) <- gets IOC.modeldef
     modSts         <- gets IOC.modsts
     envb           <- filterEnvCtoEnvB
     (maybt',envb') <- lift $ runStateT (Behave.behAfterRef modSts (Set.unions outsyncs)) envb
     writeEnvBtoEnvC envb'
     case maybt' of
     { Nothing  -> do return $ False
     ; Just bt' -> do modify $ \env -> env { IOC.modsts = bt' }
                      return $ True
     }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

