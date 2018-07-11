{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ProcDef
where

import           Control.DeepSeq
import           GHC.Generics    (Generic)
import           TorXakis.Sort              (Resettable)

import           BehExprDefs
import           ChanId

import           VarId

data  ProcDef        =  ProcDef    [ChanId] [VarId] BExpr
     deriving (Eq,Ord,Read,Show, Generic, NFData)

instance Resettable ProcDef

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
