{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Calculation of Free Variables
--
-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE ViewPatterns #-}
module FreeVar
( freeVars
, isClosed
)
where

import qualified Data.List as List
import qualified Data.Map  as Map

import TxsDefs
import Utils


-- ----------------------------------------------------------------------------------------- --
-- FreeVar: ValExpr

-- | Determine the free variables in Value Expression.
freeVars :: (Variable v) => ValExpr v -> [v]
freeVars (view -> Vfunc _fid vexps)        =  List.nub $ concatMap freeVars vexps
freeVars (view -> Vcstr _cid vexps)        =  List.nub $ concatMap freeVars vexps
freeVars (view -> Viscstr _cid vexp)       =  freeVars vexp
freeVars (view -> Vaccess _cid _p vexp)    =  freeVars vexp
freeVars (view -> Vconst _const)           =  []
freeVars (view -> Vvar vid)                =  [vid]
freeVars (view -> Vite cnrs vexp1 vexp2)   =  List.nub $ concatMap freeVars cnrs ++
                                                 freeVars vexp1 ++ freeVars vexp2
freeVars (view -> Venv ve vexp)            =  List.nub $ concatMap freeVars (Map.elems ve) ++
                                                ( freeVars vexp \\\ Map.keys ve )
freeVars (view -> Vequal vexp1 vexp2)      =  List.nub $ freeVars vexp1 ++ freeVars vexp2
freeVars (view -> Vpredef _kd _fid vexps)  =  List.nub $ concatMap freeVars vexps
freeVars (view -> Verror _str)             =  []


-- ----------------------------------------------------------------------------------------- --
-- | Is Value Expression closed?
isClosed :: (Variable v) => ValExpr v -> Bool
isClosed vexp
   =  null $ freeVars vexp

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

