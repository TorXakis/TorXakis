{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module FuncDef
where

import GHC.Generics (Generic)
import Control.DeepSeq

import VarId
import ValExprDefs

data  FuncDef        = FuncDef    [VarId] VExpr
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
