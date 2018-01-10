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

-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module LPEfunc
( 
  lpeTransformFunc
)

where

-- ----------------------------------------------------------------------------------------- --
-- import


import Control.Monad.State
import Data.Functor.Identity

import           Data.Maybe
-- import           Data.Monoid

-- import TranslatedProcDefs

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
import qualified Id

import LPE


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


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

