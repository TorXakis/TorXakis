{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BehExprDefs
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
module BehExprDefs
( BExprView(..)
, BExpr
, view
, ActOffer(..)
, Offer(..)
, ChanOffer(..)
, Trans(..)
, (~~)
, stop
, actionPref
, guard
, choice
, parallel
, enable
, disable
, interrupt
, procInst
, hide
, valueEnv
, stAut
)
where

import qualified Data.Set        as Set

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

import           ChanId
import           Id
import           ProcId
import           SortOf
import           StatId
import           VarEnv
import           VarId

-- | BExprView: the public view of Behaviour Expression `BExpr`
data BExprView = Stop
               | ActionPref  ActOffer BExpr
               | Guard       VExpr BExpr
               | Choice      [BExpr]
               | Parallel    [ChanId] [BExpr]
               | Enable      BExpr [ChanOffer] BExpr
               | Disable     BExpr BExpr
               | Interrupt   BExpr BExpr
               | ProcInst    ProcId [ChanId] [VExpr]
               | Hide        [ChanId] BExpr
               | ValueEnv    VEnv BExpr
               | StAut       StatId VEnv [Trans]
  deriving (Eq,Ord,Read,Show, Generic, NFData, Data)
instance Resettable BExprView

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
instance Resettable BExpr

stop :: BExpr
stop = BExpr Stop

actionPref :: ActOffer -> BExpr -> BExpr
actionPref a b = BExpr (ActionPref a b)

guard :: VExpr -> BExpr -> BExpr
guard v b = BExpr (Guard v b)

choice :: [BExpr] -> BExpr
choice = BExpr . Choice

parallel :: [ChanId] -> [BExpr] -> BExpr
parallel cs bs = BExpr (Parallel cs bs)

enable :: BExpr -> [ChanOffer] -> BExpr -> BExpr
enable b1 cs b2 = BExpr (Enable b1 cs b2)

disable :: BExpr -> BExpr -> BExpr
disable b1 b2 = BExpr (Disable b1 b2)

interrupt :: BExpr -> BExpr -> BExpr
interrupt b1 b2 = BExpr (Interrupt b1 b2)

procInst :: ProcId -> [ChanId] -> [VExpr] -> BExpr
procInst p cs vs = BExpr (ProcInst p cs vs)

hide :: [ChanId] -> BExpr -> BExpr
hide cs b = BExpr (Hide cs b)

valueEnv :: VEnv -> BExpr -> BExpr
valueEnv v b = BExpr (ValueEnv v b)

stAut :: StatId -> VEnv -> [Trans] -> BExpr
stAut s v ts = BExpr (StAut s v ts)

-- | ActOffer
-- Offer on multiple channels with constraints
data ActOffer = ActOffer
  { offers     :: Set.Set Offer      -- PvdL -- why not? -- Map ChanId [ChanOffer]
  , hiddenvars :: Set.Set VarId
  , constraint :: VExpr
  } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Resettable ActOffer

-- | Offer
-- Offer on a single channel (with multiple values)
data Offer = Offer
  { chanid     :: ChanId
  , chanoffers :: [ChanOffer]
  } deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Resettable Offer

-- | Channel Offer
-- Offer of a single value
data  ChanOffer     = Quest  VarId
                    | Exclam VExpr
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Resettable ChanOffer
instance SortOf ChanOffer where
  sortOf (Quest (VarId _nm _uid vs)) =  vs
  sortOf (Exclam vexp)               =  sortOf vexp

-- | symbolic transitions
data  Trans         = Trans  { from     :: StatId
                             , actoffer :: ActOffer
                             , update   :: VEnv
                             , to       :: StatId
                             }
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Resettable Trans

-- * Functions on behavior expressions.

-- | Equality modulo unique id's. Compare two behavior expressions for equality
-- ignoring the differences in identifiers.
(~~) :: BExpr -> BExpr -> Bool
be0 ~~ be1 = reset be0 == reset be1
