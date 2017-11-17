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

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- TorXakis imports
import           TxsDefs  hiding (subst)
import qualified TxsDefs

-- | Expressions that support substitution of variables for expressions.
class Subst e where
    -- | Substitution function.
    subst :: TxsDefs.VEnv                -- ^ Mapping from variable id's to
                                         -- expressions on those variable id's.
          -> Map FuncId (FuncDef VarId) -- ^ Mapping of function identifiers.
                                         -- to their definitions.
          -> e                           -- ^ Input expression.
          -> e

instance (Ord e,Subst e) => Subst [e] where
    subst ve fis = map (subst ve fis)

instance (Ord e,Subst e) => Subst (Set.Set e) where
    subst ve fis = Set.map (subst ve fis)

instance Subst BExpr where

    subst _ _ Stop = Stop

    subst ve fdefs (ActionPref (ActOffer offs cnrs) bexp) =
        ActionPref (ActOffer (subst ve fdefs offs)
                             (subst ve fdefs cnrs))
                   (subst ve fdefs bexp)

    subst ve fdefs (Guard cnrs bexp) =
        Guard (subst ve fdefs cnrs) (subst ve fdefs bexp)

    subst ve fdefs (Choice bexps) =
        Choice (subst ve fdefs bexps)

    subst ve fdefs (Parallel chids bexps) =
        Parallel chids (map (subst ve fdefs) bexps)

    subst ve fdefs (Enable bexp1 choffs bexp2) =
        Enable (subst ve fdefs bexp1)
               (subst ve fdefs choffs)
               (subst ve fdefs bexp2)

    subst ve fdefs (Disable bexp1 bexp2) =
        Disable (subst ve fdefs bexp1) (subst ve fdefs bexp2)

    subst ve fdefs (Interrupt bexp1 bexp2) =
        Interrupt (subst ve fdefs bexp1) (subst ve fdefs bexp2)

    subst ve fdefs (ProcInst pid chans vexps) =
        ProcInst pid chans (subst ve fdefs vexps)

    subst ve fdefs (Hide chids bexp) =
        Hide chids (subst ve fdefs bexp)

    subst ve fdefs (ValueEnv venv bexp) =
        subst ve fdefs (subst venv fdefs bexp)

    subst ve fdefs (StAut stid venv trns) =
        StAut stid (Map.map (subst ve fdefs) venv) trns

instance Subst Offer where
    subst ve fdefs (Offer chid choffs) = Offer chid (subst ve fdefs choffs)

instance Subst ChanOffer where
    subst _  _ (Quest vid)       = Quest vid
    subst ve fdefs (Exclam vexp) = Exclam (subst ve fdefs vexp)

instance Subst VExpr where
    subst = TxsDefs.subst
