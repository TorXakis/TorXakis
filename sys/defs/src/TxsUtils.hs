{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- ----------------------------------------------------------------------------------------- --
module TxsUtils

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Some Utilities for TxsDefs
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

where

import qualified Data.Map    as Map
import qualified Data.Set    as Set

import qualified FreeMonoidX as FMX
import           FuncId
import           StdTDefs
import           TxsDefs

-- ----------------------------------------------------------------------------------------- --
-- identifiers: signatures, binding


sig :: Ident -> Ident
sig ( IdSort   (SortId nm _uid          ) ) = IdSort   (SortId nm 0          )
sig ( IdCstr   (CstrId nm _uid ca cs    ) ) = IdCstr   (CstrId nm 0 ca cs    )
sig ( IdFunc   (FuncId nm _uid fa fs    ) ) = IdFunc   (FuncId nm 0 fa fs    )
sig ( IdProc   (ProcId nm _uid pc pv pe ) ) = IdProc   (ProcId nm 0 pc pv pe )
sig ( IdChan   (ChanId nm _uid cs       ) ) = IdChan   (ChanId nm 0 cs       )
sig ( IdVar    (VarId  nm _uid vs       ) ) = IdVar    (VarId  nm 0 vs       )
sig ( IdStat   (StatId nm _uid pid      ) ) = IdStat   (StatId nm 0 pid      )
sig ( IdModel  (ModelId  nm _uid        ) ) = IdModel  (ModelId  nm 0        )
sig ( IdPurp   (PurpId   nm _uid        ) ) = IdPurp   (PurpId   nm 0        )
sig ( IdGoal   (GoalId   nm _uid        ) ) = IdGoal   (GoalId   nm 0        )
sig ( IdMapper (MapperId nm _uid        ) ) = IdMapper (MapperId nm 0        )
sig ( IdCnect  (CnectId  nm _uid        ) ) = IdCnect  (CnectId  nm 0        )

doubles :: Eq a => [a] -> [a]
doubles []     =  []
doubles (x:xs) =  if x `elem` xs then x:doubles xs else doubles xs

bindOnName :: Name -> [Ident] -> [Ident]
bindOnName nm =  filter (\i -> TxsDefs.name i == nm)

bindOnSig :: Ident -> [Ident] -> [Ident]
bindOnSig i  =  filter (\d -> sig d == sig i)

bindOnUnid :: Int -> [Ident] -> [Ident]
bindOnUnid uid =  filter (\i -> TxsDefs.unid i == uid)

-- ----------------------------------------------------------------------------------------- --
-- scopeMerge globals locals: merge globals and locals; locals take prededence


scopeMerge :: [Ident] -> [Ident] -> [Ident]
scopeMerge []     ls  =  ls
scopeMerge (g:gs) ls  =  if sig g `elem` map sig ls
                           then scopeMerge gs ls
                           else scopeMerge gs (g:ls)

-- ----------------------------------------------------------------------------------------- --
-- combineWEnv :  combine Walue Environments;  where second takes precedence

combineWEnv :: (Variable v) => WEnv v -> WEnv v -> WEnv v
combineWEnv we1 we2
  =  let  we1' = Map.toList we1
          we2' = Map.toList we2
      in  Map.fromList $ [ (vid1,wal1) | (vid1,wal1) <- we1', vid1 `Map.notMember` we2 ]
                         ++ we2'

-- ----------------------------------------------------------------------------------------- --
-- check use of functions for use in SMT:
-- to/fromString to/fromXml takeWhile takeWhileNot dropWhile dropWhileNot
-- and transitive closure


checkENDECdefs :: TxsDefs -> [FuncId]
checkENDECdefs tdefs
  =  Set.toList $ Set.unions $ map (checkENDECdef tdefs) (TxsDefs.elems tdefs)


checkENDECdef :: TxsDefs -> TxsDef -> Set.Set FuncId
checkENDECdef tdefs tdef
  =  let endecs = allENDECfuncs tdefs
      in case tdef of
         { DefProc  (ProcDef _ _ bexp)      -> Set.fromList (usedFids bexp) `Set.intersection` endecs
         ; DefModel (ModelDef _ _ _ bexp)   -> Set.fromList (usedFids bexp) `Set.intersection` endecs
         ; DefPurp  (PurpDef _ _ _ gls)     -> Set.unions (map (Set.fromList . usedFids . snd) gls)
                                                                  `Set.intersection` endecs
         ; DefMapper (MapperDef _ _ _ bexp) -> Set.fromList (usedFids bexp) `Set.intersection` endecs
         ; _                                -> Set.empty
         }

-- ----------------------------------------------------------------------------------------- --

baseENDECfuncs :: TxsDefs -> Set.Set FuncId
baseENDECfuncs tdefs
  =  Set.fromList $ funcId_takeWhile : funcId_takeWhileNot
                    : funcId_dropWhile : funcId_dropWhileNot
                    : [ fid
                      | fid@FuncId{ FuncId.name = nm } <- Map.keys (funcDefs tdefs)
                      , (nm == "toString") || (nm  == "fromString") ||
                        (nm == "toXml") || (nm == "fromXml")
                      ]


allENDECfuncs :: TxsDefs -> Set.Set FuncId
allENDECfuncs tdefs
  =  Set.fromList [ fid
                  | fid <- Map.keys (funcDefs tdefs)
                  , not $ Set.null $ funcCallsClosure tdefs (Set.singleton fid)
                                          `Set.intersection` baseENDECfuncs tdefs
                  ]


funcCallsClosure :: TxsDefs -> Set.Set FuncId -> Set.Set FuncId
funcCallsClosure tdefs fids
  =  let newcalls = Set.unions $ map (funcCalls tdefs) (Set.toList fids)
      in if  newcalls `Set.isSubsetOf` fids
           then fids
           else funcCallsClosure  tdefs $ fids `Set.union` newcalls


funcCalls :: TxsDefs -> FuncId -> Set.Set FuncId
funcCalls tdefs fid
  =  case Map.lookup fid (funcDefs tdefs) of
     { Just (FuncDef _vids vexp) -> Set.fromList $ usedFids vexp
     ; _                         -> Set.empty
     }

-- ----------------------------------------------------------------------------------------- --

class UsedFids t
  where
    usedFids :: t -> [FuncId]


instance UsedFids BExpr
  where
    usedFids  Stop                          =  []
    usedFids (ActionPref actoff bexp)       =  usedFids actoff ++ usedFids bexp
    usedFids (Guard vexps bexp)             =  usedFids vexps ++ usedFids bexp
    usedFids (Choice bexps)                 =  usedFids bexps
    usedFids (Parallel _chids bexps)        =  usedFids bexps
    usedFids (Enable bexp1 choffs bexp2)    =  usedFids bexp1 ++ usedFids choffs
                                                              ++ usedFids bexp2
    usedFids (Disable bexp1 bexp2)          =  usedFids bexp1 ++ usedFids bexp2
    usedFids (Interrupt bexp1 bexp2)        =  usedFids bexp1 ++ usedFids bexp2
    usedFids (ProcInst _pid _chids vexps)   =  usedFids vexps
    usedFids (Hide _chids bexp)             =  usedFids bexp
    usedFids (ValueEnv ve bexp)             =  usedFids (Map.elems ve) ++ usedFids bexp
    usedFids (StAut _stid ve transs)        =  usedFids (Map.elems ve) ++ usedFids transs


instance UsedFids Trans
  where
    usedFids (Trans _fr actoff upd _to)  =  usedFids actoff ++ usedFids (Map.elems upd)


instance UsedFids ActOffer
  where
    usedFids (ActOffer offs cnrs)  =  usedFids (concatMap chanoffers (Set.toList offs))
                                     ++ usedFids cnrs


instance UsedFids ChanOffer
  where
    usedFids (Quest _vid)  =  []
    usedFids (Exclam vexp) =  usedFids vexp


instance UsedFids VExpr
  where
    usedFids (view -> Vfunc fid vexps)          =  fid : usedFids vexps
    usedFids (view -> Vcstr _cid vexps)         =  usedFids vexps
    usedFids (view -> Viscstr _cid vexp)        =  usedFids vexp
    usedFids (view -> Vaccess _cid _p vexp)     =  usedFids vexp
    usedFids (view -> Vconst _const)            =  []
    usedFids (view -> Vvar _v)                  =  []
    usedFids (view -> Vite cond tb fb)          =  usedFids [cond, tb, fb]
    usedFids (view -> Vsum s)                   =  concatMap usedFids (FMX.distinctTermsT s)
    usedFids (view -> Vproduct p)               =  concatMap usedFids (FMX.distinctTermsT p)
    usedFids (view -> Vdivide t n)              =  usedFids t ++ usedFids n
    usedFids (view -> Vmodulo t n)              =  usedFids t ++ usedFids n
    usedFids (view -> Vgez v)                   =  usedFids v
    usedFids (view -> Vequal vexp1 vexp2)       =  usedFids vexp1 ++ usedFids vexp2
    usedFids (view -> Vand vexps)               =  concatMap usedFids (Set.toList vexps)
    usedFids (view -> Vnot vexp)                =  usedFids vexp
    usedFids (view -> Vlength vexp)             =  usedFids vexp
    usedFids (view -> Vat s p)                  =  usedFids s ++ usedFids p
    usedFids (view -> Vconcat vexps)            =  concatMap usedFids vexps
    usedFids (view -> Vstrinre s r)             =  usedFids s ++ usedFids r
    usedFids (view -> Vpredef _k fid vexps)     =  fid : usedFids vexps
    usedFids (view -> Vany _s)                  =  []
    usedFids (view -> Verror _s)                =  []
    usedFids _                                  =  error "usedFids: item not in view"


instance (UsedFids t) => UsedFids [t]
  where
    usedFids  =  concatMap usedFids
