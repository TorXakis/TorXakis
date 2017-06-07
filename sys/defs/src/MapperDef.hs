{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


module MapperDef
where

import qualified Data.Set as Set

import BehExprDefs
import ChanId

data  MapperDef      = MapperDef  [ChanId] [ChanId] [Set.Set ChanId] BExpr  -- ins, outs, syncs
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

