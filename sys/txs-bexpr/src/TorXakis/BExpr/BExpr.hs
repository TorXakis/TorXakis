{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.BExpr.BExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module introduces definitions related to behaviour expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module TorXakis.BExpr.BExpr
(
  -- * Behaviour Expression type and view
  BExprView(..)
, BExpr(..)
  -- Action Offer
, ActOffer(..)
  -- Channel Offer
, ChanOffer(..)
)
where

import           Control.DeepSeq
import           Control.Monad
import           Data.Data
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           GHC.Generics        (Generic)

import           TorXakis.BExpr.ExitKind
import           TorXakis.ChanDef
import           TorXakis.ProcSignature
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.VarDef

-- | BExprView: the public view of Behaviour Expression `BExpr`
data BExprView = ActionPref  ActOffer BExpr
               | Guard       (ValExpr MinimalVarDef) BExpr
               | Choice      (Set.Set BExpr)
               | Parallel    (Set.Set ChanDef) [BExpr] -- actually (MultiSet.MultiSet BExpr) but that has lousy performance (due to sorting which needs more evaluation?)
               | Enable      BExpr [ChanOffer] BExpr
               | Disable     BExpr BExpr
               | Interrupt   BExpr BExpr
               | ProcInst    ProcSignature [ChanDef] [ValExpr MinimalVarDef]
               | Hide        (Set.Set ChanDef) BExpr
  deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | BExpr: behaviour expression
--
-- 1. User can't directly construct BExpr (such that invariants will always hold)
--
-- 2. User can still pattern match on BExpr using 'BExprView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype BExpr = BExpr {
            -- | View on Behaviour Expression
            view :: BExprView
        }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | ActOffer
-- Offer on multiple channels with constraints
data ActOffer = ActOffer
  { offers     :: Map.Map ChanDef [ChanOffer]
  , hiddenvars :: Set.Set MinimalVarDef
  , constraint :: ValExpr MinimalVarDef
  } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Channel Offer
-- Offer of a single value
data  ChanOffer     = Quest  MinimalVarDef
                    | Exclam (ValExpr MinimalVarDef)
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance HasSort ChanOffer where
    getSort (Quest  var)    = getSort var
    getSort (Exclam vexpr)  = getSort vexpr

-- | ExitKind related to ChanDef ChanOffers tuple
toExitKind :: (ChanDef , [ChanOffer]) -> ExitKind
toExitKind (cd, cs) | cd == chanExit     = Exit (map getSort cs)
toExitKind (cd, _)  | cd == chanIstep    = NoExit
toExitKind (cd, _)  | cd == chanQstep    = Hit
toExitKind (cd, _)  | cd == chanHit      = Hit
toExitKind (cd, _)  | cd == chanMiss     = Hit
toExitKind _                             = NoExit

instance HasExitKind ActOffer where
    getExitKind a = case foldM (<<+>>) NoExit (map toExitKind (Map.toList (offers a))) of
                        Right v -> v
                        Left e  -> error ("Smart constructor created invalid ActOffer " ++ show e)

instance HasExitKind BExprView where
    getExitKind (ActionPref  a b)   = case getExitKind a <<+>> getExitKind b of
                                            Right v -> v
                                            Left e  -> error ("Smart constructor created invalid ActionPref " ++ show e)
    getExitKind (Guard _ b)         = getExitKind b
    getExitKind (Choice s)          = case foldM (<<+>>) NoExit (map getExitKind (Set.toList s)) of
                                            Right v -> v
                                            Left e  -> error ("Smart constructor created invalid Choice " ++ show e)
    getExitKind (Parallel _ l)      = case foldM (<<->>) NoExit (map getExitKind l) of
                                            Right v -> v
                                            Left e  -> error ("Smart constructor created invalid Parallel " ++ show e)
    getExitKind (Enable _ _ b)      = getExitKind b
    getExitKind (Disable a b)       = case getExitKind a <<+>> getExitKind b of
                                            Right v -> v
                                            Left e  -> error ("Smart constructor created invalid Disable " ++ show e)
    getExitKind (Interrupt a _)     = getExitKind a
    getExitKind (ProcInst ps _ _)   = exitKind ps
    getExitKind (Hide _ b)          = getExitKind b

instance HasExitKind BExpr where
    getExitKind = getExitKind . TorXakis.BExpr.BExpr.view
