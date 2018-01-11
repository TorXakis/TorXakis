{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SortDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Sort Definition
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module SortDef
where

import Control.DeepSeq
import Data.Data
import GHC.Generics (Generic)

import Id

-- | SortDef has no information 
data  SortDef        = SortDef
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Identifiable SortDef where
    getId _ = Nothing

instance Resettable SortDef where
    reset = id
