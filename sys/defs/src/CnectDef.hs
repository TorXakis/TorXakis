{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module CnectDef
where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

import           ValExpr         (Resettable)

import           ConnectionDefs

data  CnectDef       = CnectDef   CnectType [ConnDef]
     deriving (Eq,Ord,Read,Show, Generic, NFData)

instance Resettable CnectDef

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
