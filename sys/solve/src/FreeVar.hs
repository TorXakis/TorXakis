{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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
import qualified Data.Set  as Set

import Product
import Sum
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
freeVars (view -> Vite cond vexp1 vexp2)   =  List.nub $ freeVars cond ++
                                                         freeVars vexp1 ++ 
                                                         freeVars vexp2
freeVars (view -> Venv ve vexp)            =  List.nub $ concatMap freeVars (Map.elems ve) ++
                                                ( freeVars vexp \\\ Map.keys ve )
freeVars (view -> Vsum s)                  =  List.nub $ concatMap freeVars (Sum.distinctTerms s)
freeVars (view -> Vproduct p)              =  List.nub $ concatMap freeVars (Product.distinctTerms p)
freeVars (view -> Vdivide t n)             =  List.nub $ freeVars t ++ freeVars n
freeVars (view -> Vmodulo t n)             =  List.nub $ freeVars t ++ freeVars n
freeVars (view -> Vgez vexp)               =  freeVars vexp
freeVars (view -> Vequal vexp1 vexp2)      =  List.nub $ freeVars vexp1 ++ freeVars vexp2
freeVars (view -> Vnot vexp)               =  freeVars vexp
freeVars (view -> Vand vexps)              =  List.nub $ concatMap freeVars (Set.toList vexps)
freeVars (view -> Vpredef _kd _fid vexps)  =  List.nub $ concatMap freeVars vexps
freeVars (view -> Verror _str)             =  []
freeVars _                                 = error "freeVars - Item not in view"


-- ----------------------------------------------------------------------------------------- --
-- | Is Value Expression closed?
isClosed :: (Variable v) => ValExpr v -> Bool
isClosed vexp
   =  null $ freeVars vexp

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
