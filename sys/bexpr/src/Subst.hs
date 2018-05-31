{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
-- ----------------------------------------------------------------------------------------- --

module Subst

-- ----------------------------------------------------------------------------------------- --
--
-- Substitution of Value (Expressions) for Variables
-- Substitution can be partial, ie. some free variables are not Substituted,
-- so types of substituted and substituting must be equal
--
-- ----------------------------------------------------------------------------------------- --

where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- TorXakis imports
import           BehExprDefs
import           FuncDef
import           FuncId
import qualified ValExpr
import           VarEnv
import           VarId

-- | Expressions that support substitution of variables for expressions.
class Subst e where
    -- | Substitution function.
    subst :: VEnv                   -- ^ Mapping from variable id's to
                                            -- expressions on those variable id's.
          -> Map.Map FuncId (FuncDef VarId) -- ^ Mapping of function identifiers.
                                            -- to their definitions.
          -> e                              -- ^ Input expression.
          -> e

instance Subst e => Subst [e] where
    subst ve fis = map (subst ve fis)

instance (Ord e, Subst e) => Subst (Set.Set e) where
    subst ve fis = Set.map (subst ve fis)

instance Subst BExpr where
    subst ve _     | Map.null ve = id
    subst ve fdefs               = subst' ve fdefs . view
    
subst' :: VEnv -> Map.Map FuncId (FuncDef VarId) -> BExprView -> BExpr
subst' ve fdefs (ActionPref (ActOffer offs hidvars cnrs) bexp) =
    actionPref (ActOffer (subst ve fdefs offs)
                         hidvars
                         (subst ve fdefs cnrs))
               (subst ve fdefs bexp)

subst' ve fdefs (Guard cnrs bexp) =
    guard (subst ve fdefs cnrs) (subst ve fdefs bexp)

subst' ve fdefs (Choice bexps) =
    choice (subst ve fdefs bexps)

subst' ve fdefs (Parallel chids bexps) =
    parallel chids (map (subst ve fdefs) bexps)

subst' ve fdefs (Enable bexp1 choffs bexp2) =
    enable (subst ve fdefs bexp1)
           (subst ve fdefs choffs)
           (subst ve fdefs bexp2)

subst' ve fdefs (Disable bexp1 bexp2) =
    disable (subst ve fdefs bexp1) (subst ve fdefs bexp2)

subst' ve fdefs (Interrupt bexp1 bexp2) =
    interrupt (subst ve fdefs bexp1) (subst ve fdefs bexp2)

subst' ve fdefs (ProcInst pid chans vexps) =
    procInst pid chans (subst ve fdefs vexps)

subst' ve fdefs (Hide chids bexp) =
    hide chids (subst ve fdefs bexp)

subst' ve fdefs (ValueEnv venv bexp) =
    subst ve fdefs (subst venv fdefs bexp)

subst' ve fdefs (StAut stid venv trns) =
    stAut stid (Map.map (subst ve fdefs) venv) trns

instance Subst Offer where
    subst ve fdefs (Offer chid choffs) = Offer chid (subst ve fdefs choffs)

instance Subst ChanOffer where
    subst _  _ (Quest vid)       = Quest vid
    subst ve fdefs (Exclam vexp) = Exclam (subst ve fdefs vexp)

instance Subst VExpr where
    subst = ValExpr.subst
