{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module MapperDef
where

import GHC.Generics (Generic)
import Control.DeepSeq

import qualified Data.Set as Set

import BehExprDefs
import ChanId

data  MapperDef      = MapperDef  [ChanId] [ChanId] [Set.Set ChanId] BExpr  -- ins, outs, syncs
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
