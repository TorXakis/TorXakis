{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-----------------------------------------------------------------------------
-- |
-- Module      :  LPEfunc
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  carsten.ruetz, jan.tretmans
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
module LPEfunc
(
  lpeTransformFunc,
  lpeParFunc,
  lpeHideFunc,
  preGNFEnableFunc,
  preGNFDisableFunc,
  gnfFunc,
  preGNFFunc,
  eqProcDef,
  eqProcDefs
)

where

-- ----------------------------------------------------------------------------------------- --
-- import


import Control.Monad.State
import Data.Functor.Identity
import qualified Data.Map as Map

import TranslatedProcDefs
import TxsDefs

import VarId
import Name

import qualified EnvData
import qualified EnvBasic            as EnvB
import Id

import LPE


-- ----------------------------------------------------------------------------------------- --
-- function lpeTransform for testing

type IOL   =  StateT EnvL Identity

data EnvL  =  EnvL { uniqid   :: Id.Id
                   , chanofferss :: Map.Map (Name, Int) VarId
                   , messgs   :: [EnvData.Msg]
                   }

instance EnvB.EnvB IOL
  where
     newUnid        = newUnid
     putMsgs        = putMsgs
     setChanoffers  = setChanoffers
     getChanoffers  = getChanoffers

newUnid :: IOL Id.Id
newUnid  =  do
     uniqid' <- gets uniqid
     modify $ \envl -> envl { uniqid = uniqid' + 1 }
     return $ uniqid' + 1

putMsgs :: [EnvData.Msg] -> IOL ()
putMsgs msg  =  do
     messgs' <- gets messgs
     modify $ \envl -> envl { messgs = messgs' ++ msg }

setChanoffers :: Map.Map (Name, Int) VarId -> IOL ()
setChanoffers map' =
    modify $ \envl -> envl { chanofferss = map' }

getChanoffers :: IOL (Map.Map (Name, Int) VarId)
getChanoffers = gets chanofferss

lpeTransformFunc :: BExpr
                 -> ProcDefs
                 -> Maybe (BExpr, ProcDef)
lpeTransformFunc procInst' procDefs'
  =  let envl = EnvL 0 (Map.fromList []) []
      in evalState (lpeTransform procInst' procDefs') envl



-- lpePar :: (EnvB.EnvB envb) => BExpr -> TranslatedProcDefs -> ProcDefs -> envb(BExpr, ProcDefs)
lpeParFunc :: BExpr -> TranslatedProcDefs -> ProcDefs -> (BExpr, ProcDefs)
lpeParFunc bexpr translatedProcDefs procDefs' =
  let envl = EnvL 0 (Map.fromList []) []
   in evalState (lpePar bexpr translatedProcDefs procDefs') envl


-- lpeHide :: (EnvB.EnvB envb) => BExpr -> TranslatedProcDefs -> ProcDefs -> envb(BExpr, ProcDefs)
lpeHideFunc :: BExpr -> Map.Map (Name, Int) VarId -> TranslatedProcDefs -> ProcDefs -> (BExpr, ProcDefs)
lpeHideFunc bexpr _chanOffers translatedProcDefs procDefs' =
  let envl = EnvL 0 (Map.fromList []) []
   in evalState (lpeHide bexpr translatedProcDefs procDefs') envl

-- preGNFEnable :: (EnvB.EnvB envb) => BExpr -> TranslatedProcDefs -> ProcDefs -> envb(BExpr, ProcDefs)
preGNFEnableFunc :: BExpr -> Map.Map (Name, Int) VarId -> TranslatedProcDefs -> ProcDefs -> (BExpr, ProcDefs)
preGNFEnableFunc bexpr _chanOffers translatedProcDefs procDefs' =
  let envl = EnvL 0 (Map.fromList []) []
   in evalState (preGNFEnable bexpr translatedProcDefs procDefs') envl

-- preGNFDisable :: (EnvB.EnvB envb) => BExpr -> TranslatedProcDefs -> ProcDefs -> envb(BExpr, ProcDefs)
preGNFDisableFunc :: BExpr -> Map.Map (Name, Int) VarId -> TranslatedProcDefs -> ProcDefs -> (BExpr, ProcDefs)
preGNFDisableFunc bexpr _chanOffers translatedProcDefs procDefs' =
  let envl = EnvL 0 (Map.fromList []) []
   in evalState (preGNFDisable bexpr translatedProcDefs procDefs') envl


-- gnf :: (EnvB.EnvB envb) => ProcId -> TranslatedProcDefs -> ProcDefs -> envb (ProcDefs)
gnfFunc :: ProcId -> TranslatedProcDefs -> ProcDefs -> (ProcDefs, Map.Map ProcId [ProcId])
gnfFunc procId translatedProcDefs procDefs' =
 let envl = EnvL 0 (Map.fromList []) []
  in evalState (gnf procId Map.empty translatedProcDefs procDefs') envl


  -- preGNF :: (EnvB.EnvB envb) => ProcId -> TranslatedProcDefs -> ProcDefs -> envb ProcDefs
  -- preGNF procId translatedProcDefs procDefs' = do
-- preGNF :: (EnvB.EnvB envb) => ProcId -> TranslatedProcDefs -> ProcDefs -> envb(ProcDefs)
preGNFFunc :: ProcId -> TranslatedProcDefs -> ProcDefs -> ProcDefs
preGNFFunc procId translatedProcDefs procDefs' =
 let envl = EnvL 0 (Map.fromList []) []
  in evalState (preGNF procId translatedProcDefs procDefs') envl




eqProcDef :: Maybe(BExpr, ProcDef) -> Maybe(BExpr, ProcDef) -> Bool
eqProcDef (Just(bexprL, procDefL)) (Just(bexprR, procDefR)) =
    (bexprL ~~ bexprR) && eqProcDef' procDefL procDefR
eqProcDef _ _ = False


eqProcDef' :: ProcDef -> ProcDef -> Bool
eqProcDef' procDefL procDefR =
    reset' procDefL == reset' procDefR
    where
      reset' :: ProcDef -> ProcDef
      reset' (ProcDef chanIds varIds bexpr) =
        ProcDef (reset chanIds) (reset varIds) (reset bexpr)


eqProcDefs :: ProcDefs -> ProcDefs -> Bool
eqProcDefs procDefsL procDefsR =
    let l = Map.toList procDefsL
        r = Map.toList procDefsR
        zipped = zip l r
        compared = map comp zipped in
    length l == length r && and compared
    where
      comp (l,r) = eq_elem l r
      eq_elem (procIdL, procDefL) (procIdR, procDefR) =
        reset procIdL == reset procIdR && eqProcDef' procDefL procDefR

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
