{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeVars
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Free Variables in Value Expression related functionality.
-----------------------------------------------------------------------------
module TorXakis.FreeVars
( freeVars
, isClosed
)
where
import qualified Data.Map               as Map
import qualified Data.Set               as Set


import           TorXakis.ValExpr
import           TorXakis.VarDef

-- | Determine the free variables in Value Expression.
freeVars :: (VarDef v) => ValExpr v -> Set.Set v
freeVars = Set.fromList . freeVarsList
    where
        freeVarsList :: (VarDef v) => ValExpr v -> [v]
        freeVarsList = freeVarsListView . view
        
        freeVarsListView :: (VarDef v) => ValExprView v -> [v]
        freeVarsListView  Vconst{}             = []
        freeVarsListView (Vvar v)              = [v]
        freeVarsListView (Vequal v1 v2)        = concatMap freeVarsList [v1, v2]
        freeVarsListView (Vite c t f)          = concatMap freeVarsList [c, t, f]
        freeVarsListView (Vfunc _ as)          = concatMap freeVarsList as
        freeVarsListView (Vpredef _ as)        = concatMap freeVarsList as
        freeVarsListView (Vnot v)              = freeVarsList v
        freeVarsListView (Vand s)              = concatMap freeVarsList (Set.toList s)
        freeVarsListView (Vdivide t n)         = concatMap freeVarsList [t, n]
        freeVarsListView (Vmodulo t n)         = concatMap freeVarsList [t, n]
        freeVarsListView (Vsum m)              = concatMap freeVarsList (Map.keys m)
        freeVarsListView (Vproduct m)          = concatMap freeVarsList (Map.keys m)
        freeVarsListView (Vgez v)              = freeVarsList v
        freeVarsListView (Vlength v)           = freeVarsList v
        freeVarsListView (Vat s p)             = concatMap freeVarsList [s, p]
        freeVarsListView (Vconcat vs)          = concatMap freeVarsList vs
        freeVarsListView (Vstrinre s r)        = concatMap freeVarsList [s, r]
        freeVarsListView (Vcstr _ _ vs)        = concatMap freeVarsList vs
        freeVarsListView (Viscstr _ _ v)       = freeVarsList v
        freeVarsListView (Vaccess _ _ _ _ v)   = freeVarsList v

-- | Is Value Expression closed?
-- A closed value expression has no free variables.
isClosed :: (VarDef v) => ValExpr v -> Bool
isClosed = Set.null . freeVars