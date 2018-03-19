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
-- TODO: make sure these warnings are removed.
-- TODO: also check the hlint warnings!
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module LPEfunc
(
  lpeTransformFunc,
  lpeParFunc,
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

import           Data.Maybe
-- import           Data.Monoid

-- import TranslatedProcDefs

import TranslatedProcDefs
import TxsDefs
-- import ConstDefs
-- import StdTDefs (stdSortTable)

-- import ChanId
-- import ProcId
-- import SortId
-- import VarId

-- import BehExprDefs
-- import ValExpr
-- import qualified TxsUtils

import qualified EnvData
import qualified EnvBasic            as EnvB
import Id

import LPE
import qualified Data.Map as Map


-- ----------------------------------------------------------------------------------------- --
-- function lpeTransform for testing

type IOL   =  StateT EnvL Identity

data EnvL  =  EnvL { uniqid   :: Id.Id
                   , messgs   :: [EnvData.Msg]
                   }

instance EnvB.EnvB IOL
  where
     newUnid  =  newUnid
     putMsgs  =  putMsgs

newUnid :: IOL Id.Id
newUnid  =  do
     uniqid' <- gets uniqid
     modify $ \envl -> envl { uniqid = uniqid' + 1 }
     return $ uniqid' + 1

putMsgs :: [EnvData.Msg] -> IOL ()
putMsgs msg  =  do
     messgs' <- gets messgs
     modify $ \envl -> envl { messgs = messgs' ++ msg }

lpeTransformFunc :: BExpr
                 -> ProcDefs
                 -> Maybe (BExpr, ProcDef)
lpeTransformFunc procInst procDefs
  =  let envl = EnvL 0 []
      in evalState (lpeTransform procInst procDefs) envl



-- lpePar :: (EnvB.EnvB envb) => BExpr -> TranslatedProcDefs -> ProcDefs -> envb(BExpr, ProcDefs)
lpeParFunc :: BExpr -> TranslatedProcDefs -> ProcDefs -> (BExpr, ProcDefs)
lpeParFunc bexpr translatedProcDefs procDefs =
  let envl = EnvL 0 []
   in evalState (lpePar bexpr translatedProcDefs procDefs) envl



-- gnf :: (EnvB.EnvB envb) => ProcId -> TranslatedProcDefs -> ProcDefs -> envb (ProcDefs)
gnfFunc :: ProcId -> TranslatedProcDefs -> ProcDefs -> ProcDefs
gnfFunc procId translatedProcDefs procDefs =
 let envl = EnvL 0 []
  in evalState (gnf procId translatedProcDefs procDefs) envl


-- preGNF :: (EnvB.EnvB envb) => ProcId -> TranslatedProcDefs -> ProcDefs -> envb(ProcDefs)
preGNFFunc :: ProcId -> TranslatedProcDefs -> ProcDefs -> ProcDefs
preGNFFunc procId translatedProcDefs procDefs =
 let envl = EnvL 0 []
  in evalState (preGNF procId translatedProcDefs procDefs) envl




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
    and compared
    where
      comp (l,r) = eq_elem l r
      eq_elem (procIdL, procDefL) (procIdR, procDefR) =
        reset procIdL == reset procIdR && eqProcDef' procDefL procDefR

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
