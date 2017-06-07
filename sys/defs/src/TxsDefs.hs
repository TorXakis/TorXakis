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
module TxsDefs 
( TxsDefs(..)
, TxsDefs.empty
, TxsDefs.fromList
, TxsDefs.toList
, TxsDefs.lookup
, TxsDefs.keys
, TxsDefs.elems
, TxsDefs.union
, TxsDefs.insert
, TxsDef(..)
, Variable(..)
, ValExpr
, cstrFunc
, cstrCstr
, cstrConst
, cstrVar
, cstrIte
, cstrEnv
, cstrEqual
, cstrPredef
, cstrError
, view
, ValExprView(..)
, PredefKind(..)
, VarEnv
, VExpr
, VEnv
, NoId(NoId)
, SortDef(SortDef)
, SortId(SortId)
, CstrDef(CstrDef)
, CstrId(CstrId, cstrargs, cstrsort)
, FuncDef(FuncDef)
, FuncId(FuncId, funcargs, funcsort)
, ProcDef(ProcDef)
, ProcId(ProcId)
, ChanId(ChanId)
, VarId(VarId)
, StatId(StatId)
, ModelDef(ModelDef)
, ModelId(ModelId)
, PurpDef(PurpDef)
, PurpId(PurpId)
, GoalId(GoalId)
, MapperDef(MapperDef)
, MapperId(MapperId)
, CnectDef(CnectDef)
, CnectId(CnectId)
, ExitSort(..)
, module X
)
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow ( (***) )

import BehExprDefs as X
import ConnectionDefs as X
import ConstDefs as X
import Ident as X
import Name as X
import TxsDef as X
import Variable as X

import NoId
import SortDef
import SortId
import CstrDef
import CstrId
import FuncDef
import FuncId
import ProcDef
import ProcId
import ChanId
import VarId
import StatId
import ModelDef
import ModelId
import PurpDef
import PurpId
import GoalId
import MapperDef
import MapperId
import CnectDef
import CnectId

import ValExprDefs
import ValExprImpls

-- ----------------------------------------------------------------------------------------- --
-- torxakis definitions


data  TxsDefs  =  TxsDefs { noDefs      :: Map.Map NoId ()
                          , sortDefs    :: Map.Map SortId SortDef
                          , cstrDefs    :: Map.Map CstrId CstrDef
                          , funcDefs    :: Map.Map FuncId FuncDef
                          , procDefs    :: Map.Map ProcId ProcDef
                          , chanDefs    :: Map.Map ChanId ()
                          , varDefs     :: Map.Map VarId ()
                          , statDefs    :: Map.Map StatId ()
                          , modelDefs   :: Map.Map ModelId ModelDef
                          , purpDefs    :: Map.Map PurpId PurpDef
                          , goalDefs    :: Map.Map GoalId ()
                          , mapperDefs  :: Map.Map MapperId MapperDef
                          , cnectDefs   :: Map.Map CnectId CnectDef
                          }
                  deriving (Eq,Ord,Read,Show)

empty :: TxsDefs
empty = TxsDefs  Map.empty
                 Map.empty
                 Map.empty
                 Map.empty
                 Map.empty
                 Map.empty
                 Map.empty
                 Map.empty
                 Map.empty
                 Map.empty
                 Map.empty
                 Map.empty
                 Map.empty
                
lookup :: Ident -> TxsDefs -> Maybe TxsDef
lookup (IdNo s) txsdefs = case Map.lookup s (noDefs txsdefs) of
                                Nothing -> Nothing
                                Just _  -> Just DefNo
lookup (IdSort s) txsdefs = case Map.lookup s (sortDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefSort d)
lookup (IdCstr s) txsdefs = case Map.lookup s (cstrDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefCstr d)
lookup (IdFunc s) txsdefs = case Map.lookup s (funcDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefFunc d)
lookup (IdProc s) txsdefs = case Map.lookup s (procDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefProc d)
lookup (IdChan s) txsdefs = case Map.lookup s (chanDefs txsdefs) of
                                Nothing -> Nothing
                                Just _  -> Just DefNo
lookup (IdVar s) txsdefs = case Map.lookup s (varDefs txsdefs) of
                                Nothing -> Nothing
                                Just _  -> Just DefNo
lookup (IdStat s) txsdefs = case Map.lookup s (statDefs txsdefs) of
                                Nothing -> Nothing
                                Just _  -> Just DefNo
lookup (IdModel s) txsdefs = case Map.lookup s (modelDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefModel d)
lookup (IdPurp s) txsdefs = case Map.lookup s (purpDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefPurp d)
lookup (IdGoal s) txsdefs = case Map.lookup s (goalDefs txsdefs) of
                                Nothing -> Nothing
                                Just _  -> Just DefNo
lookup (IdMapper s) txsdefs = case Map.lookup s (mapperDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefMapper d)
lookup (IdCnect s) txsdefs = case Map.lookup s (cnectDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefCnect d)
                                
insert :: Ident -> TxsDef -> TxsDefs -> TxsDefs
insert (IdNo s)     DefNo         t     = t { noDefs     = Map.insert s () (noDefs t)     }
insert (IdSort s)   (DefSort d)   t     = t { sortDefs   = Map.insert s d  (sortDefs t)   }
insert (IdCstr s)   (DefCstr d)   t     = t { cstrDefs   = Map.insert s d  (cstrDefs t)   }
insert (IdFunc s)   (DefFunc d)   t     = t { funcDefs   = Map.insert s d  (funcDefs t)   }
insert (IdProc s)   (DefProc d)   t     = t { procDefs   = Map.insert s d  (procDefs t)   }
insert (IdChan s)   DefNo         t     = t { chanDefs   = Map.insert s () (chanDefs t)   }
insert (IdVar s)    DefNo         t     = t { varDefs    = Map.insert s () (varDefs t)    }
insert (IdStat s)   DefNo         t     = t { statDefs   = Map.insert s () (statDefs t)   }
insert (IdModel s)  (DefModel d)  t     = t { modelDefs  = Map.insert s d  (modelDefs t)  }
insert (IdPurp s)   (DefPurp d)   t     = t { purpDefs   = Map.insert s d  (purpDefs t)   }
insert (IdGoal s)   DefNo         t     = t { goalDefs   = Map.insert s () (goalDefs t)   }    
insert (IdMapper s) (DefMapper d) t     = t { mapperDefs = Map.insert s d  (mapperDefs t) }
insert (IdCnect s)  (DefCnect d)  t     = t { cnectDefs  = Map.insert s d  (cnectDefs t)  }
insert i            d             _     = error $ "Unknown insert\nident = " ++ show i ++ "\ndefinition = " ++ show d    
    
fromList :: [(Ident, TxsDef)] -> TxsDefs
fromList = foldl addElem empty
  where
    addElem :: TxsDefs -> (Ident,TxsDef) -> TxsDefs
    addElem t (k,v) = insert k v t
    
    
toList :: TxsDefs -> [(Ident, TxsDef)]
toList t =      map (IdNo Control.Arrow.*** (const DefNo))      (Map.toList (noDefs t))
            ++  map (IdSort Control.Arrow.*** DefSort)          (Map.toList (sortDefs t))
            ++  map (IdCstr Control.Arrow.*** DefCstr)          (Map.toList (cstrDefs t))
            ++  map (IdFunc Control.Arrow.*** DefFunc)          (Map.toList (funcDefs t))
            ++  map (IdProc Control.Arrow.*** DefProc)          (Map.toList (procDefs t))
            ++  map (IdChan Control.Arrow.*** (const DefNo))    (Map.toList (chanDefs t))
            ++  map (IdVar Control.Arrow.*** (const DefNo))     (Map.toList (varDefs t))
            ++  map (IdStat Control.Arrow.*** (const DefNo))    (Map.toList (statDefs t))
            ++  map (IdModel Control.Arrow.*** DefModel)        (Map.toList (modelDefs t))
            ++  map (IdPurp Control.Arrow.*** DefPurp)          (Map.toList (purpDefs t))
            ++  map (IdGoal Control.Arrow.*** (const DefNo))    (Map.toList (goalDefs t))
            ++  map (IdMapper Control.Arrow.*** DefMapper)      (Map.toList (mapperDefs t))
            ++  map (IdCnect Control.Arrow.*** DefCnect)        (Map.toList (cnectDefs t))
            
            
keys :: TxsDefs -> [Ident]
keys t =        map IdNo        (Map.keys (noDefs t))
            ++  map IdSort      (Map.keys (sortDefs t))
            ++  map IdCstr      (Map.keys (cstrDefs t))
            ++  map IdFunc      (Map.keys (funcDefs t))
            ++  map IdProc      (Map.keys (procDefs t))
            ++  map IdChan      (Map.keys (chanDefs t))
            ++  map IdVar       (Map.keys (varDefs t))
            ++  map IdStat      (Map.keys (statDefs t))
            ++  map IdModel     (Map.keys (modelDefs t))
            ++  map IdPurp      (Map.keys (purpDefs t))
            ++  map IdGoal      (Map.keys (goalDefs t))
            ++  map IdMapper    (Map.keys (mapperDefs t))
            ++  map IdCnect     (Map.keys (cnectDefs t))
            
elems :: TxsDefs -> [TxsDef]
elems t =       map (const DefNo)   (Map.elems (noDefs t))
            ++  map DefSort         (Map.elems (sortDefs t))
            ++  map DefCstr         (Map.elems (cstrDefs t))
            ++  map DefFunc         (Map.elems (funcDefs t))
            ++  map DefProc         (Map.elems (procDefs t))
            ++  map (const DefNo)   (Map.elems (chanDefs t))
            ++  map (const DefNo)   (Map.elems (varDefs t))
            ++  map (const DefNo)   (Map.elems (statDefs t))
            ++  map DefModel        (Map.elems (modelDefs t))
            ++  map DefPurp         (Map.elems (purpDefs t))
            ++  map (const DefNo)   (Map.elems (goalDefs t))
            ++  map DefMapper       (Map.elems (mapperDefs t))
            ++  map DefCnect        (Map.elems (cnectDefs t))

            
union :: TxsDefs -> TxsDefs -> TxsDefs
union a b = TxsDefs
                (Map.union (noDefs a)    (noDefs b)     )
                (Map.union (sortDefs a)  (sortDefs b)   )
                (Map.union (cstrDefs a)  (cstrDefs b)   )
                (Map.union (funcDefs a)  (funcDefs b)   )
                (Map.union (procDefs a)  (procDefs b)   )
                (Map.union (chanDefs a)  (chanDefs b)   )
                (Map.union (varDefs a)   (varDefs b)    )
                (Map.union (statDefs a)  (statDefs b)   )
                (Map.union (modelDefs a) (modelDefs b)  )
                (Map.union (purpDefs a)  (purpDefs b)   )
                (Map.union (goalDefs a)  (goalDefs b)   )
                (Map.union (mapperDefs a)(mapperDefs b) )
                (Map.union (cnectDefs a) (cnectDefs b)  )

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

