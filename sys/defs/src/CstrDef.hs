{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module CstrDef
where

import GHC.Generics (Generic)
import Control.DeepSeq

import FuncId

data  CstrDef       = CstrDef    FuncId [FuncId]       -- constructor_check [field_selectors]
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
