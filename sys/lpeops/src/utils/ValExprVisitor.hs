{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ValExprVisitor
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
module ValExprVisitor (
ValExprVisitorOutput(..),
visitValExpr,
visitValExprM,
ValExprVisitor,
ValExprVisitorM,
defaultValExprVisitor,
defaultValExprVisitorM,
tryDefaultValExprVisitor
) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.State as MonadState
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified FreeMonoidX as FMX
import qualified Constant
import qualified TxsDefs
import qualified FuncDef
import qualified FuncId
import qualified VarId
import           ValExpr

data ValExprVisitorOutput t = ValExprVisitorOutput { expression :: TxsDefs.VExpr
                                                   , multiplicity :: Integer
                                                   , customData :: t
                                                   }
-- ValExprVisitorOutput

instance DeepSeq.NFData t => DeepSeq.NFData (ValExprVisitorOutput t) where
    rnf ValExprVisitorOutput { expression = e, multiplicity = m, customData = c } = DeepSeq.rnf e `seq` DeepSeq.rnf m `seq` DeepSeq.rnf c
-- DeepSeq.NFData t => DeepSeq.NFData (ValExprVisitorOutput t)

type ValExprVisitor t = [ValExprVisitorOutput t] -> TxsDefs.VExpr -> ValExprVisitorOutput t
type ValExprVisitorM m t = Monad.Monad m => [ValExprVisitorOutput t] -> TxsDefs.VExpr -> m (ValExprVisitorOutput t)

visitValExpr :: ValExprVisitor t -> TxsDefs.VExpr -> ValExprVisitorOutput t
visitValExpr f expr = MonadState.evalState (visitValExprM (\xs x -> return (f xs x)) expr) ()

-- Function that applies a visitor pattern to the given value expression.
-- Children are always evaluated before the parent, and the result is a composition
-- that is dependent on the evaluated children and the parent.
visitValExprM :: Monad.Monad m => ValExprVisitorM m t -> TxsDefs.VExpr -> m (ValExprVisitorOutput t)
visitValExprM f expr = do
    let visitValExprM1 = visitValExprMK f 1
    case expr of
      (view -> Vconst (Constant.Ccstr _ vexps)) ->
          do newVExps <- Monad.mapM (visitValExprM1 . cstrConst) vexps
             f newVExps expr
      (view -> Vconst _) ->
          f [] expr
      (view -> Vvar _) ->
          f [] expr
      (view -> Vfunc _fid vexps) ->
          do newVExps <- Monad.mapM visitValExprM1 vexps
             f newVExps expr
      (view -> Vcstr _cid vexps) ->
          do newVExps <- Monad.mapM visitValExprM1 vexps
             f newVExps expr
      (view -> Viscstr _cid vexp) ->
          do newVExp <- visitValExprM1 vexp
             f [newVExp] expr
      (view -> Vaccess _cid _n _p vexp) ->
          do newVExp <- visitValExprM1 vexp
             f [newVExp] expr
      (view -> Vite cond vexp1 vexp2) ->
          do newCond <- visitValExprM1 cond
             newVExp1 <- visitValExprM1 vexp1
             newVExp2 <- visitValExprM1 vexp2
             f [newCond, newVExp1, newVExp2] expr
      (view -> Vdivide t n) ->
          visitValExprWithDivision f t n expr
      (view -> Vmodulo t n) ->
          visitValExprWithDivision f t n expr
      (view -> Vgez v) ->
          do newV <- visitValExprM1 v
             f [newV] expr
      (view -> Vsum s) ->
          do newVExps <- Monad.mapM (\(v, k) -> visitValExprMK f k v) (FMX.toDistinctAscOccurListT s)
             f newVExps expr
      (view -> Vproduct p) ->
          do newVExps <- Monad.mapM (\(v, k) -> visitValExprMK f k v) (FMX.toDistinctAscOccurListT p)
             f newVExps expr
      (view -> Vequal vexp1 vexp2) ->
          do newVExp1 <- visitValExprM1 vexp1
             newVExp2 <- visitValExprM1 vexp2
             f [newVExp1, newVExp2] expr
      (view -> Vand vexps) ->
          do newVExps <- Monad.mapM visitValExprM1 (Set.toList vexps)
             f newVExps expr
      (view -> Vnot vexp) ->
          do newVExp <- visitValExprM1 vexp
             f [newVExp] expr
      (view -> Vlength vexp) ->
          do newVExp <- visitValExprM1 vexp
             f [newVExp] expr
      (view -> Vat s p) ->
          do newS <- visitValExprM1 s
             newP <- visitValExprM1 p
             f [newS, newP] expr
      (view -> Vconcat vexps) ->
          do newVExps <- Monad.mapM visitValExprM1 vexps
             f newVExps expr
      (view -> Vstrinre s r) ->
          do newS <- visitValExprM1 s
             newR <- visitValExprM1 r
             f [newS, newR] expr
      (view -> Vpredef _kd _fid vexps) ->
          do newVExps <- Monad.mapM visitValExprM1 vexps
             f newVExps expr
-- visitValExprM

-- Here, we think we know better (which we do) and
-- override the multiplicity of the result of the visit with the specified value:
visitValExprMK :: Monad.Monad m => ValExprVisitorM m t -> Integer -> TxsDefs.VExpr -> m (ValExprVisitorOutput t)
visitValExprMK f k expr = do
    expr' <- visitValExprM f expr
    return (expr' { multiplicity = k })
-- visitValExprMK

-- Separate function because otherwise hlint complains:
visitValExprWithDivision :: Monad.Monad m => ValExprVisitorM m t -> TxsDefs.VExpr -> TxsDefs.VExpr -> TxsDefs.VExpr -> m (ValExprVisitorOutput t)
visitValExprWithDivision f t n expr = do
    let visitValExprM1 = visitValExprMK f 1
    newT <- visitValExprM1 t
    newN <- visitValExprM1 n
    f [newT, newN] expr
-- visitValExprWithDivision

defaultValExprVisitor :: t -> ValExprVisitor t
defaultValExprVisitor defaultDat subExps expr = MonadState.evalState (defaultValExprVisitorM defaultDat subExps expr) ()

defaultValExprVisitorM :: Monad.Monad m => t -> ValExprVisitorM m t
defaultValExprVisitorM defaultDat subExps expr = do
    let expr' = case expr of
                  (view -> Vconst (Constant.Ccstr cid _)) -> cstrCstr cid (map expression subExps)
                  (view -> Vconst _)                      -> expr
                  (view -> Vvar _)                        -> expr
                  (view -> Vfunc fid _)                   -> cstrFunc emptyFis fid (map expression subExps)
                  (view -> Vcstr cid _)                   -> cstrCstr cid (map expression subExps)
                  (view -> Viscstr cid _)                 -> cstrIsCstr cid (expression (head subExps))
                  (view -> Vaccess cid n p _)             -> cstrAccess cid n p (expression (head subExps))
                  (view -> Vite{})                        -> cstrITE (expression (head subExps)) (expression (subExps !! 1)) (expression (subExps !! 2))
                  (view -> Vdivide _ _)                   -> cstrDivide (expression (head subExps)) (expression (subExps !! 1))
                  (view -> Vmodulo _ _)                   -> cstrModulo (expression (head subExps)) (expression (subExps !! 1))
                  (view -> Vgez _)                        -> cstrGEZ (expression (head subExps))
                  (view -> Vsum _)                        -> cstrSum (FMX.fromOccurListT (map (\x -> (expression x, multiplicity x)) subExps))
                  (view -> Vproduct _)                    -> cstrProduct (FMX.fromOccurListT (map (\x -> (expression x, multiplicity x)) subExps))
                  (view -> Vequal _ _)                    -> cstrEqual (expression (head subExps)) (expression (subExps !! 1))
                  (view -> Vand _)                        -> cstrAnd (Set.fromList (map expression subExps))
                  (view -> Vnot _)                        -> cstrNot (expression (head subExps))
                  (view -> Vlength _)                     -> cstrLength (expression (head subExps))
                  (view -> Vat _ _)                       -> cstrAt (expression (head subExps)) (expression (subExps !! 1))
                  (view -> Vconcat _)                     -> cstrConcat (map expression subExps)
                  (view -> Vstrinre _ _)                  -> cstrStrInRe (expression (head subExps)) (expression (subExps !! 1))
                  (view -> Vpredef kd fid _)              -> cstrPredef kd fid (map expression subExps)
    return (ValExprVisitorOutput expr' 1 defaultDat)
  where
    emptyFis :: Map.Map FuncId.FuncId (FuncDef.FuncDef VarId.VarId)
    emptyFis = Map.empty :: Map.Map FuncId.FuncId (FuncDef.FuncDef VarId.VarId)
-- defaultValExprVisitorM

tryDefaultValExprVisitor :: (DeepSeq.NFData t) => t -> [ValExprVisitorOutput t] -> TxsDefs.VExpr -> IO (Either Exception.ErrorCall (ValExprVisitorOutput t))
tryDefaultValExprVisitor defaultDat subExps expr = Exception.try (Exception.evaluate (DeepSeq.force (defaultValExprVisitor defaultDat subExps expr)))





