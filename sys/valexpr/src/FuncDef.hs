{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Function Definition
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module FuncDef
where

import GHC.Generics (Generic)
import Control.DeepSeq

import ValExprDefs

-- | Data structure to store the information of a Function Definition:
-- * A list of variables
-- * A body (possibly using the variables)
data  FuncDef v      = FuncDef    [v] (ValExpr v)
     deriving (Eq, Ord, Read, Show, Generic, NFData)
