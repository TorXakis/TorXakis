{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
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
import CstrDef
import FuncDef
import MapperDef
import ModelDef
import ProcDef
import PurpDef
import SortDef
-- ----------------------------------------------------------------------------------------- --
-- torxakis definitions

data  TxsDef        =  DefNo
                     | DefSort      SortDef
                     | DefCstr      CstrDef
                     | DefFunc      FuncDef
                     | DefProc      ProcDef
                     | DefModel     ModelDef
                     | DefPurp      PurpDef
                     | DefMapper    MapperDef
                     | DefCnect     CnectDef
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

