{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module PurpDef
where

import GHC.Generics (Generic)
import Control.DeepSeq

import qualified Data.Set as Set

import BehExprDefs
import ChanId
import GoalId

data  PurpDef        = PurpDef    [Set.Set ChanId] [Set.Set ChanId]
                                  [Set.Set ChanId] [(GoalId,BExpr)]
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

