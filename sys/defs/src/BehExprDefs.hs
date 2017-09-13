{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

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

import qualified Data.Set as Set

import GHC.Generics (Generic)
import Control.DeepSeq

import ChanId
import ProcId
import StatId
import VarId
import ValExprDefs

-- | Behaviour Expression
data  BExpr         = Stop
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
     deriving (Eq,Ord,Read,Show, Generic, NFData)


-- | ActOffer
-- Offer on multiple channels with constraints
data  ActOffer      =  ActOffer { offers      :: Set.Set Offer      -- PvdL -- why not? -- Map ChanId [ChanOffer]
                                , constraints :: [VExpr]
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData)


-- | Offer 
-- Offer on a single channel (with multiple values)
data  Offer         =  Offer { chanid     :: ChanId
                             , chanoffers :: [ChanOffer]
                             }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- | Channel Offer
-- Offer of a single value
data  ChanOffer     = Quest  VarId
                    | Exclam VExpr
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- | symbolic transitions 
data  Trans         = Trans  { from     :: StatId
                             , actoffer :: ActOffer
                             , update   :: VEnv
                             , to       :: StatId
                             }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
