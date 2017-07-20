{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module ProcDef
where

import GHC.Generics (Generic)
import Control.DeepSeq

import BehExprDefs
import ChanId
import VarId

data  ProcDef        =  ProcDef    [ChanId] [VarId] BExpr
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

