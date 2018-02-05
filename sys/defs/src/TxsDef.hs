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
import ProcDef
import PurpDef
import Sort

import VarId

-- | torxakis definitions
data  TxsDef = DefADT       ADTDef
             | DefFunc      (FuncDef VarId)
             | DefProc      ProcDef
             | DefModel     ModelDef
             | DefPurp      PurpDef
             | DefMapper    MapperDef
             | DefCnect     CnectDef
             | DefChan
             | DefVar
             | DefStat
             | DefGoal
     deriving (Eq,Ord,Read,Show)
