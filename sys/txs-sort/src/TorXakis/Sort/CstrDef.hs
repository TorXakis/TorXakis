{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Sort.CstrDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structure for constructors
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module TorXakis.Sort.CstrDef
where

import GHC.Generics (Generic)
import Control.DeepSeq

import TorXakis.ValExpr.FuncId
import TorXakis.Sort.Id (Resettable)

-- | A constructor has a `isCstr` functions, and a list of accessors functions.
data  CstrDef       = CstrDef    FuncId [FuncId]       -- constructor_check [field_selectors]
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- | CstrDef is instance of @Resettable@.
instance Resettable CstrDef

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
