{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


module CstrDef
where

import FuncId

data  CstrDef       = CstrDef    FuncId [FuncId]       -- constructor_check [field_selectors]
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

