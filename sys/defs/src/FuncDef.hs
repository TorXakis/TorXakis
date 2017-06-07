{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


module FuncDef
where

import qualified Data.Set as Set

import VarId
import ValExprDefs


data  FuncDef        = FuncDef    [VarId] VExpr
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

