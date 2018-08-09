{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Simplifiable
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- 'TorXakis' expressions that can be simplified.
--------------------------------------------------------------------------------
module TorXakis.Compiler.Simplifiable
    (simplify)
where

import           Control.Lens    (over)
import           Data.Data.Lens  (uniplate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)

import qualified BehExprDefs     as BExpr
import           CnectId         (CnectId)
import           FreeMonoidX     (FreeMonoidX, mapTerms)
import           FuncDef         (FuncDef (FuncDef))
import           FuncId          (FuncId (FuncId))
import           FuncTable       (FuncTable, Signature (Signature), toMap)
import           GoalId          (GoalId)
import           ProcId          (ProcId)
import           Product         (ProductTerm (ProductTerm))
import           PurpId          (PurpId)
import           Sum             (SumTerm (SumTerm))
import           TxsDefs         (ActOffer (ActOffer), BExpr, BExprView (ActionPref, Guard, ProcInst, StAut, ValueEnv),
                                  ChanOffer (Exclam, Quest),
                                  CnectDef (CnectDef),
                                  ConnDef (ConnDfroW, ConnDtoW),
                                  MapperDef (MapperDef), MapperId,
                                  ModelDef (ModelDef), ModelId, Offer (Offer),
                                  ProcDef (ProcDef), PurpDef (PurpDef),
                                  Trans (Trans), actionPref, guard, procInst,
                                  stAut, valueEnv)
import           ValExpr         (ValExpr,
                                  ValExprView (Vfunc, Vite, Vproduct, Vsum),
                                  cstrITE, cstrProduct, cstrSum)
import qualified ValExpr
import           VarId           (VarId)

-- | 'TorXakis' expressions that can be simplified.
class Simplifiable e where
    -- | Simplify the expressions by applying the functions defined in the function table.
    --
    simplify :: FuncTable VarId
             -> [Text] -- ^ Symbols that are allowed to be simplified (used for
                       -- compliance with old TorXakis compiler)
             -> e
             -> e

instance Simplifiable (ValExpr VarId) where
    simplify ft fns ex@(ValExpr.view -> Vfunc (FuncId n _ aSids rSid) vs) =
        -- NOTE: For now make the simplification only if "n" is a predefined
        -- symbol. Once compliance with the current `TorXakis` compiler is not
        -- needed we can remove this constraint and simplify further. However,
        -- care must be taken with recursive functions to avoid infinite calls
        -- to `simplify`.
        if n `elem` fns
        then fromMaybe ex $ do
            sh <- Map.lookup n (toMap ft)
            h  <- Map.lookup (Signature aSids rSid) sh
            return $ h (simplify ft fns <$> vs)
        else ex
    simplify ft fns (ValExpr.view -> Vite ex0 ex1 ex2) =
        cstrITE
        (simplify ft fns ex0)
        (simplify ft fns ex1)
        (simplify ft fns ex2)
    simplify ft fns (ValExpr.view -> Vsum terms) =
        cstrSum (simplify ft fns terms)
    simplify ft fns (ValExpr.view -> Vproduct terms) =
        cstrProduct (simplify ft fns terms)
    simplify ft fns x                          =
        over uniplate (simplify ft fns) x

instance Simplifiable FuncId where
    simplify _ _ = id

instance Simplifiable ModelId where
    simplify _ _ = id

instance Simplifiable MapperId where
    simplify _ _ = id

instance Simplifiable (FuncDef VarId) where
    simplify ft fns (FuncDef vs ex) = FuncDef vs (simplify ft fns ex)

instance (Simplifiable a, Simplifiable b) => Simplifiable (a, b) where
    simplify ft fns (a, b) = (simplify ft fns a, simplify ft fns b)

instance (Simplifiable a) => Simplifiable [a] where
    simplify ft fns = fmap (simplify ft fns)

instance (Simplifiable a, Ord a) => Simplifiable (Set a) where
    simplify ft fns = Set.fromList . fmap (simplify ft fns) . Set.toList

instance (Simplifiable k, Simplifiable v, Ord k) => Simplifiable (Map k v) where
    simplify ft fns = Map.fromList . simplify ft fns . Map.toList

instance Simplifiable ProcId where
    simplify _ _ = id

instance Simplifiable ProcDef where
    simplify ft fns (ProcDef cs vs be) = ProcDef cs vs (simplify ft fns be)

instance Simplifiable ModelDef where
    simplify ft fns (ModelDef ins outs syncs be) = ModelDef ins outs syncs (simplify ft fns be)

instance Simplifiable MapperDef where
    simplify ft fns (MapperDef ins outs syncs be) = MapperDef ins outs syncs (simplify ft fns be)

instance Simplifiable BExpr where
    simplify ft fns (BExpr.view -> ActionPref ao be)
        = actionPref (simplify ft fns ao) (simplify ft fns be)
    simplify ft fns (BExpr.view -> ValueEnv env be)
        = valueEnv (simplify ft fns env) (simplify ft fns be)
    simplify ft fns (BExpr.view -> ProcInst p cs vs)
        = procInst p cs (simplify ft fns vs)
    simplify ft fns (BExpr.view -> Guard g be)
        = guard (simplify ft fns g) (simplify ft fns be)
    simplify ft fns (BExpr.view -> StAut st vEnv trs)
        = stAut st (simplify ft fns vEnv) (simplify ft fns trs)
    simplify ft fns be = over uniplate (simplify ft fns) be

instance Simplifiable ActOffer where
    simplify ft fns (ActOffer aos hv c) = ActOffer (simplify ft fns aos) hv (simplify ft fns c)

instance Simplifiable Offer where
    simplify ft fns (Offer cId os) = Offer cId (simplify ft fns os)

instance Simplifiable ChanOffer where
    simplify _  _   x@(Quest _)   = x
    simplify ft fns (Exclam vexp) = Exclam (simplify ft fns vexp)

instance Simplifiable VarId where
    simplify _ _ = id

instance Simplifiable Trans where
    simplify ft fns (Trans from ofr upd to) = Trans from ofr' upd' to
        where
          ofr' = simplify ft fns ofr
          upd' = simplify ft fns upd

instance Simplifiable PurpId where
    simplify _ _ = id

instance Simplifiable PurpDef where
    simplify ft fns (PurpDef is os ys gs) =
        PurpDef is os ys (simplify ft fns gs)

instance Simplifiable GoalId where
    simplify _ _ = id

instance Simplifiable CnectId where
    simplify _ _ = id

instance Simplifiable CnectDef where
    simplify ft fns (CnectDef t cs) = CnectDef t (simplify ft fns cs)

instance Simplifiable ConnDef where
    simplify ft fns (ConnDtoW c h p vs e) = ConnDtoW c h p vs (simplify ft fns e)
    simplify ft fns (ConnDfroW c h p v es) = ConnDfroW c h p v (simplify ft fns es)

instance (Ord a, Simplifiable a) => Simplifiable (FreeMonoidX a) where
    simplify ft fns = mapTerms (simplify ft fns)

instance (Simplifiable a) => Simplifiable (SumTerm a) where
    simplify ft fns (SumTerm a) = SumTerm (simplify ft fns a)

instance (Simplifiable a) => Simplifiable (ProductTerm a) where
    simplify ft fns (ProductTerm a) = ProductTerm (simplify ft fns a)
