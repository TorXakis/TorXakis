{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
--
-- TorXakis Interal Data Type Definitions:
--
--   *  General     :  general definitions
--   *  Frontend    :  (static) abstract syntax definitions for the TorXakis Language (.txs)
--   *  Connections :  connections to outside world
--
-- ----------------------------------------------------------------------------------------- --
module TxsDef
where

import CnectDef
import FuncDef
import MapperDef
import ModelDef
import Name
import ProcDef
import PurpDef
import Sort

import VarId

-- | torxakis definitions
data  TxsDef = -- DefADT (ADTDef Name) TODO: this is an bad trick for making a
               -- function polymorphic, and we should get rid of it. This ADT
               -- should be removed as soon as possible.
               DefFunc      (FuncDef VarId)
             | DefProc      ProcDef
             | DefModel     ModelDef
             | DefPurp      PurpDef
             | DefMapper    MapperDef
             | DefCnect     CnectDef
             | DefChan
             | DefVar
             | DefStat
             | DefGoal
     deriving (Eq,Read,Show)
