{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module Ident

where
import Name

import ChanId
import CnectId
import CstrId
import FuncId
import GoalId
import MapperId
import ModelId
import ProcId
import PurpId
import SortId
import StatId
import VarId
import Variable
-- ----------------------------------------------------------------------------------------- --
-- identifiers


data  Ident         =  IdSort   SortId
                     | IdCstr   CstrId
                     | IdFunc   FuncId
                     | IdProc   ProcId
                     | IdChan   ChanId
                     | IdVar    VarId
                     | IdStat   StatId
                     | IdModel  ModelId
                     | IdPurp   PurpId
                     | IdGoal   GoalId
                     | IdMapper MapperId
                     | IdCnect  CnectId
     deriving (Eq,Ord,Read,Show)

instance Variable Ident
  where
    vname vid =  case vid of
                 { IdVar (VarId nm uid _)   -> nm++"$$"++show uid
                 ; _                        -> error "TXS TxsDefs vname: This should not happen 1 .. \n"
                 }
    vunid vid =  case vid of 
                 { IdVar (VarId _ uid _)    -> uid
                 ; _                        -> error "TXS TxsDefs vunid: This should not happen 2 .. \n"
                 }
    vsort vid =  case vid of
                 { IdVar (VarId _ _  srt)   -> srt
                 ; _                        -> error "TXS TxsDefs vsort: This should not happen 3 .. \n"
                 }
    cstrVariable s i t = IdVar (VarId s i t)
     
name :: Ident -> Name
name (IdSort i)     = SortId.name i
name (IdCstr i)     = CstrId.name i
name (IdFunc i)     = FuncId.name i
name (IdProc i)     = ProcId.name i
name (IdChan i)     = ChanId.name i
name (IdVar i)      = VarId.name i
name (IdStat i)     = StatId.name i
name (IdModel i)    = ModelId.name i
name (IdPurp i)     = PurpId.name i
name (IdGoal i)     = GoalId.name i
name (IdMapper i)   = MapperId.name i
name (IdCnect i)    = CnectId.name i

unid :: Ident -> Int
unid (IdSort i)     = SortId.unid i
unid (IdCstr i)     = CstrId.unid i
unid (IdFunc i)     = FuncId.unid i
unid (IdProc i)     = ProcId.unid i
unid (IdChan i)     = ChanId.unid i
unid (IdVar i)      = VarId.unid i
unid (IdStat i)     = StatId.unid i
unid (IdModel i)    = ModelId.unid i
unid (IdPurp i)     = PurpId.unid i
unid (IdGoal i)     = GoalId.unid i
unid (IdMapper i)   = MapperId.unid i
unid (IdCnect i)    = CnectId.unid i

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
