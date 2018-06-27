{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ModelDef
where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

import qualified Data.Set        as Set

import           BehExprDefs
import           ChanId
import           Id              (Resettable)

data  ModelDef       = ModelDef   [Set.Set ChanId] [Set.Set ChanId] [Set.Set ChanId] BExpr
     deriving (Eq,Ord,Read,Show, Generic, NFData)

instance Resettable ModelDef

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
