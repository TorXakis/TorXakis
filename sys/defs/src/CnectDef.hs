{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


module CnectDef
where

import ConnectionDefs

data  CnectDef       = CnectDef   CnectType [ConnDef]
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

