{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
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
-- , TxsDefs.fromList
-- , TxsDefs.toList
-- , TxsDefs.lookup
-- , TxsDefs.keys
-- , TxsDefs.elems
-- , TxsDefs.union
-- , TxsDefs.insert
, VarEnv
, VExpr
, VEnv
, WEnv
, ProcDef(ProcDef)
, ProcId(ProcId)
, ChanId(ChanId)
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
import           Control.DeepSeq
import qualified Data.Map        as Map
import           GHC.Generics    (Generic)

import           BehExprDefs     as X
import           ConnectionDefs  as X

import           ChanId
import           CnectDef
import           CnectId
import           FuncDef
import           FuncId
import           GoalId
import           Identifier
import           MapperDef
import           MapperId
import           ModelDef
import           ModelId
import           ProcDef
import           ProcId
import           PurpDef
import           PurpId
import           Sort
import           StatId
import           VarEnv
import           VarId

data  TxsDefs  =  TxsDefs { adtDefs    :: Map.Map (Ref ADTDef) ADTDef
                          , funcDefs   :: Map.Map FuncId (FuncDef VarId)
                          , procDefs   :: Map.Map ProcId ProcDef
                          , chanDefs   :: Map.Map ChanId ()            -- only for parsing, not envisioned for computation
                          , varDefs    :: Map.Map VarId ()             -- local
                          , statDefs   :: Map.Map StatId ()            -- local
                          , modelDefs  :: Map.Map ModelId ModelDef
                          , purpDefs   :: Map.Map PurpId PurpDef
                          , goalDefs   :: Map.Map GoalId () --BExpr    -- local, part of PurpDefs
                          , mapperDefs :: Map.Map MapperId MapperDef
                          , cnectDefs  :: Map.Map CnectId CnectDef
                          }
                  deriving (Eq,Ord,Read,Show, Generic, NFData)

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
{-
lookup :: Ident -> TxsDefs -> Maybe TxsDef
lookup (IdFunc s) txsdefs = case Map.lookup s (funcDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefFunc d)
lookup (IdProc s) txsdefs = case Map.lookup s (procDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefProc d)
lookup (IdChan s) txsdefs = case Map.lookup s (chanDefs txsdefs) of
                                Nothing -> Nothing
                                Just _  -> Just DefChan
lookup (IdVar s) txsdefs = case Map.lookup s (varDefs txsdefs) of
                                Nothing -> Nothing
                                Just _  -> Just DefVar
lookup (IdStat s) txsdefs = case Map.lookup s (statDefs txsdefs) of
                                Nothing -> Nothing
                                Just _  -> Just DefStat
lookup (IdModel s) txsdefs = case Map.lookup s (modelDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefModel d)
lookup (IdPurp s) txsdefs = case Map.lookup s (purpDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefPurp d)
lookup (IdGoal s) txsdefs = case Map.lookup s (goalDefs txsdefs) of
                                Nothing -> Nothing
                                Just _  -> Just DefGoal
lookup (IdMapper s) txsdefs = case Map.lookup s (mapperDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefMapper d)
lookup (IdCnect s) txsdefs = case Map.lookup s (cnectDefs txsdefs) of
                                Nothing -> Nothing
                                Just d  -> Just (DefCnect d)

insert :: Ident -> TxsDef -> TxsDefs -> TxsDefs
insert (IdFunc s)   (DefFunc d)   t     = t { funcDefs   = Map.insert s d  (funcDefs t)   }
insert (IdProc s)   (DefProc d)   t     = t { procDefs   = Map.insert s d  (procDefs t)   }
insert (IdChan s)   DefChan       t     = t { chanDefs   = Map.insert s () (chanDefs t)   }
insert (IdVar s)    DefVar        t     = t { varDefs    = Map.insert s () (varDefs t)    }
insert (IdStat s)   DefStat       t     = t { statDefs   = Map.insert s () (statDefs t)   }
insert (IdModel s)  (DefModel d)  t     = t { modelDefs  = Map.insert s d  (modelDefs t)  }
insert (IdPurp s)   (DefPurp d)   t     = t { purpDefs   = Map.insert s d  (purpDefs t)   }
insert (IdGoal s)   DefGoal       t     = t { goalDefs   = Map.insert s () (goalDefs t)   }
insert (IdMapper s) (DefMapper d) t     = t { mapperDefs = Map.insert s d  (mapperDefs t) }
insert (IdCnect s)  (DefCnect d)  t     = t { cnectDefs  = Map.insert s d  (cnectDefs t)  }
insert i            d             _     = error $ "Unknown insert\nident = " ++ show i ++ "\ndefinition = " ++ show d

fromList :: [(Ident, TxsDef)] -> TxsDefs
fromList = foldl addElem empty
  where
    addElem :: TxsDefs -> (Ident,TxsDef) -> TxsDefs
    addElem t (k,v) = insert k v t


toList :: TxsDefs -> [(Ident, TxsDef)]
toList t =      map (IdFunc Control.Arrow.*** DefFunc)          (Map.toList (funcDefs t))
            ++  map (IdProc Control.Arrow.*** DefProc)          (Map.toList (procDefs t))
            ++  map (IdChan Control.Arrow.*** const DefChan)    (Map.toList (chanDefs t))
            ++  map (IdVar Control.Arrow.*** const DefVar)      (Map.toList (varDefs t))
            ++  map (IdStat Control.Arrow.*** const DefStat)    (Map.toList (statDefs t))
            ++  map (IdModel Control.Arrow.*** DefModel)        (Map.toList (modelDefs t))
            ++  map (IdPurp Control.Arrow.*** DefPurp)          (Map.toList (purpDefs t))
            ++  map (IdGoal Control.Arrow.*** const DefGoal)    (Map.toList (goalDefs t))
            ++  map (IdMapper Control.Arrow.*** DefMapper)      (Map.toList (mapperDefs t))
            ++  map (IdCnect Control.Arrow.*** DefCnect)        (Map.toList (cnectDefs t))


keys :: TxsDefs -> [Ident]
keys t =        map IdFunc      (Map.keys (funcDefs t))
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
elems t =       map DefFunc         (Map.elems (funcDefs t))
            ++  map DefProc         (Map.elems (procDefs t))
            ++  map (const DefChan) (Map.elems (chanDefs t))
            ++  map (const DefVar)  (Map.elems (varDefs t))
            ++  map (const DefStat) (Map.elems (statDefs t))
            ++  map DefModel        (Map.elems (modelDefs t))
            ++  map DefPurp         (Map.elems (purpDefs t))
            ++  map (const DefGoal) (Map.elems (goalDefs t))
            ++  map DefMapper       (Map.elems (mapperDefs t))
            ++  map DefCnect        (Map.elems (cnectDefs t))


union :: TxsDefs -> TxsDefs -> TxsDefs
union a b = TxsDefs
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
-}