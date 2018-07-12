{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PurpDef
where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

import qualified Data.Set        as Set

import           TorXakis.Sort   (Resettable)

import           BehExprDefs
import           ChanId
import           GoalId

data  PurpDef        = PurpDef    [Set.Set ChanId] [Set.Set ChanId]
                                  [Set.Set ChanId] [(GoalId,BExpr)]
     deriving (Eq,Ord,Read,Show, Generic, NFData)

instance Resettable PurpDef

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
