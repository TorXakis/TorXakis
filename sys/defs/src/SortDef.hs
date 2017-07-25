{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module SortDef
where

import GHC.Generics (Generic)
import Control.DeepSeq

-- | SortDef has no information 
data  SortDef        = SortDef
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

