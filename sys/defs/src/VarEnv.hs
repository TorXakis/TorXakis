{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
-- Variable Environment
--
-- ----------------------------------------------------------------------------------------- --
module VarEnv
( VarEnv
, VExpr
, VEnv
, WEnv
)
where
import qualified Data.Map as Map
import ConstDefs
import ValExpr
import VarId

type  VarEnv v w    = Map.Map v (ValExpr w)     -- simultaneous substitution
                                                -- all variables different
                                                -- non-recursive

type  VExpr         = ValExpr VarId

type  VEnv          = VarEnv VarId VarId

type  WEnv v        = Map.Map v Const
-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
