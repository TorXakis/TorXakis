{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  BlindSubst
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}
module BlindSubst (
restoreTdefs,
any2freshVar,
any2defaultValue,
doBlindSubst
) where

import qualified Control.Monad.State as MonadState
import qualified EnvCore as IOC
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified TxsDefs
import qualified SortOf
import qualified Constant
import qualified VarId
import qualified ValExpr
import           ValExprVisitor
import           VarFactory
import           ValFactory

-- Manipulating expressions (e.g. blind substitutions before SAT-solving) may require helper variables.
-- These variables are added to the TorXakis definitions in the environment of the monad.
-- To undo these additions, pass the original definitions to the following helper method:
restoreTdefs :: TxsDefs.TxsDefs -> IOC.IOC ()
restoreTdefs tdefs = do
    state <- MonadState.gets IOC.state
    let newState = state { IOC.tdefs = tdefs }
    MonadState.modify (\env -> env { IOC.state = newState })
-- restoreTdefs

-- Eliminates occurrences of 'ANY <sort>' by substituting fresh, free variables.
-- Also returns the previous TorXakis definitions (so that they can be restored afterwards, see above).
any2freshVar :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.TxsDefs, TxsDefs.VExpr, Set.Set VarId.VarId)
any2freshVar expr = do
    tdefs <- MonadState.gets (IOC.tdefs . IOC.state)
    visitorOutput <- visitValExprM any2FreshVarVisitorM expr
    return (tdefs, expression visitorOutput, customData visitorOutput)
  where
    any2FreshVarVisitorM :: [ValExprVisitorOutput (Set.Set VarId.VarId)] -> TxsDefs.VExpr -> IOC.IOC (ValExprVisitorOutput (Set.Set VarId.VarId))
    any2FreshVarVisitorM _ (ValExpr.view -> ValExpr.Vconst (Constant.Cany sort)) = do
        varId <- createFreshVar sort
        return (ValExprVisitorOutput (ValExpr.cstrVar varId) 1 (Set.singleton varId))
    any2FreshVarVisitorM xs x = do
        vo <- MonadState.liftIO $ tryDefaultValExprVisitor (Set.unions (map customData xs)) xs x
        case vo of
          Left _ -> do varId <- createFreshVar (SortOf.sortOf x)
                       return (ValExprVisitorOutput (ValExpr.cstrVar varId) 1 (Set.singleton varId))
          Right r -> return r
-- any2freshVar

-- Eliminates occurrences of 'ANY <sort>' by substituting default data expressions of the same sort.
any2defaultValue :: TxsDefs.TxsDefs -> TxsDefs.VExpr -> TxsDefs.VExpr
any2defaultValue tdefs expr = expression (visitValExpr any2defaultVisitor expr)
  where
    any2defaultVisitor :: [ValExprVisitorOutput ()] -> TxsDefs.VExpr -> ValExprVisitorOutput ()
    any2defaultVisitor _ (ValExpr.view -> ValExpr.Vconst (Constant.Cany sort)) = ValExprVisitorOutput (sort2defaultValue tdefs sort) 1 ()
    any2defaultVisitor xs x = defaultValExprVisitor () xs x
-- any2defaultValue

-- Applies a substitution to the given expression, introducing 'ANY <sort>' for appearing undefined expressions.
doBlindSubst :: Map.Map VarId.VarId TxsDefs.VExpr -> TxsDefs.VExpr -> IOC.IOC TxsDefs.VExpr
doBlindSubst subst expr = do
    visitorOutput <- visitValExprM substVisitor expr
    return (expression visitorOutput)
  where
    substVisitor :: [ValExprVisitorOutput ()] -> TxsDefs.VExpr -> IOC.IOC (ValExprVisitorOutput ())
    -- If we find a variable, substitute it (only if it is present in substEqs, of course):
    substVisitor _ (ValExpr.view -> ValExpr.Vvar varId) =
        case subst Map.!? varId of
          Just v -> return (ValExprVisitorOutput v 1 ())
          Nothing -> return (ValExprVisitorOutput (ValExpr.cstrVar varId) 1 ())
    -- In other cases, reconstruction of the parent expression might fail
    -- (for example because something was substituted incorrectly)
    -- in which case we return 'ANY <sort>' instead:
    substVisitor subExps parentExpr = do
        vo <- MonadState.liftIO $ tryDefaultValExprVisitor () subExps parentExpr
        case vo of
          Left _ -> return (ValExprVisitorOutput (ValExpr.cstrConst (Constant.Cany (SortOf.sortOf parentExpr))) 1 ())
          Right r -> return r
-- doBlindSubst

