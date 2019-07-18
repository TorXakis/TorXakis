{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEBlindSubst
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}
module LPEBlindSubst (
doBlindParamEqSubst,
doBlindParamEqsSubst,
doConfidentSubst,
doConfidentParamEqSubst,
doConfidentParamEqsSubst,
module BlindSubst
) where

import qualified Control.Monad as Monad
import qualified Control.Monad.State as MonadState
import qualified EnvCore as IOC
import qualified EnvData
import qualified Data.Map as Map
import qualified TxsDefs
import qualified SortOf
import qualified VarId
import qualified ValExpr
import           ValExprVisitor
import           ValFactory
import           BlindSubst
import           LPETypes
import           LPEPrettyPrint

-- Applies 'doBlindSubst' to the value of a key-value pair:
doBlindParamEqSubst :: Map.Map VarId.VarId TxsDefs.VExpr -> (VarId.VarId, TxsDefs.VExpr) -> IOC.IOC (VarId.VarId, TxsDefs.VExpr)
doBlindParamEqSubst subst (varId, expr) = do
    expr' <- doBlindSubst subst expr
    return (varId, expr')
-- doBlindParamEqSubst

-- Applies 'doBlindSubst' to each value of a key-value map:
doBlindParamEqsSubst :: Map.Map VarId.VarId TxsDefs.VExpr -> Map.Map VarId.VarId TxsDefs.VExpr -> IOC.IOC (Map.Map VarId.VarId TxsDefs.VExpr)
doBlindParamEqsSubst subst target = do
    paramEqs <- Monad.mapM (doBlindParamEqSubst subst) (Map.toList target)
    return (Map.fromList paramEqs)
-- doBlindParamEqsSubst

-- Applies a substitution to the given expression, using default data expressions for undefined expressions.
doConfidentSubst :: LPESummand -> Map.Map VarId.VarId TxsDefs.VExpr -> TxsDefs.VExpr -> IOC.IOC TxsDefs.VExpr
doConfidentSubst contextSummand subst expr = do
    txsdefs <- MonadState.gets (IOC.tdefs . IOC.state)
    visitorOutput <- visitValExprM (substVisitor txsdefs) expr
    return (expression visitorOutput)
  where
    substVisitor :: TxsDefs.TxsDefs -> [ValExprVisitorOutput ()] -> TxsDefs.VExpr -> IOC.IOC (ValExprVisitorOutput ())
    -- If we find a variable, substitute it (only if it is present in substEqs, of course):
    substVisitor _ _ (ValExpr.view -> ValExpr.Vvar varId) =
        case subst Map.!? varId of
          Just v -> return (ValExprVisitorOutput v 1 ())
          Nothing -> return (ValExprVisitorOutput (ValExpr.cstrVar varId) 1 ())
    -- In other cases, reconstruction of the parent expression might fail
    -- (for example because something was substituted incorrectly)
    -- in which case we return a default data expression of the matching sort instead:
    substVisitor tdefs subExps parentExpr = do
        vo <- MonadState.liftIO $ tryDefaultValExprVisitor () subExps parentExpr
        case vo of
          Left _ -> do let defaultValue = sort2defaultValue tdefs (SortOf.sortOf parentExpr)
                       -- Print a warning, because if this happens we should at least scratch ourselves behind our ears:
                       IOC.putMsgs [ EnvData.TXS_CORE_RUNTIME_WARNING ("WARNING: Confidently substituted " ++ showValExpr defaultValue ++ " for " ++ showValExpr parentExpr ++ showSubst subst
                                       ++ "\nExpression: " ++ showValExpr expr
                                       ++ "\nSummand: " ++ showLPESummand Map.empty contextSummand) ]
                       return (ValExprVisitorOutput defaultValue 1 ())
          Right r -> return r
-- doConfidentSubst

-- Applies 'doConfidentSubst' to the value of a key-value pair:
doConfidentParamEqSubst :: LPESummand -> Map.Map VarId.VarId TxsDefs.VExpr -> (VarId.VarId, TxsDefs.VExpr) -> IOC.IOC (VarId.VarId, TxsDefs.VExpr)
doConfidentParamEqSubst summand subst (varId, expr) = do
    expr' <- doConfidentSubst summand subst expr
    return (varId, expr')
-- doConfidentParamEqSubst

-- Applies 'doConfidentSubst' to each value of a key-value map:
doConfidentParamEqsSubst :: LPESummand -> Map.Map VarId.VarId TxsDefs.VExpr -> Map.Map VarId.VarId TxsDefs.VExpr -> IOC.IOC (Map.Map VarId.VarId TxsDefs.VExpr)
doConfidentParamEqsSubst summand subst target = do
    paramEqs <- Monad.mapM (doConfidentParamEqSubst summand subst) (Map.toList target)
    return (Map.fromList paramEqs)
-- doConfidentParamEqsSubst

