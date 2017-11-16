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

import ValExprDefs

data  FuncDef v      = FuncDef    [v] (ValExpr v)
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
