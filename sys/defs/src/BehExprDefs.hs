{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

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

module BehExprDefs

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

-- | Behaviour Expression
data BExpr = Stop
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

instance Resettable BExpr

-- | ActOffer
-- Offer on multiple channels with constraints
data ActOffer = ActOffer
  { offers     :: Set.Set Offer      -- PvdL -- why not? -- Map ChanId [ChanOffer]
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
