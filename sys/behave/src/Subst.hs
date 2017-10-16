{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleInstances #-}
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

import qualified Data.Set  as Set
import qualified Data.Map  as Map

import TxsDefs

-- ----------------------------------------------------------------------------------------- --
-- Substitution


class Subst e
  where
    subst :: TxsDefs.VEnv -> e -> e


instance (Ord e,Subst e) => Subst [e]
  where
    subst ve =  map (Subst.subst ve)


instance (Ord e,Subst e) => Subst (Set.Set e)
  where
    subst ve =  Set.map (Subst.subst ve)


-- ----------------------------------------------------------------------------------------- --
-- variable substitution in BExpr


instance Subst BExpr
  where

    subst _ Stop
      =  Stop

    subst ve (ActionPref (ActOffer offs cnrs)  bexp)
      =  ActionPref (ActOffer (Subst.subst ve offs) (Subst.subst ve cnrs)) (Subst.subst ve bexp)

    subst ve (Guard cnrs bexp)
      =  Guard (Subst.subst ve cnrs) (Subst.subst ve bexp)

    subst ve (Choice bexps)
      =  Choice (Subst.subst ve bexps)

    subst ve (Parallel chids bexps)
      =  Parallel chids (map (Subst.subst ve) bexps)
    
    subst ve (Enable bexp1 choffs bexp2)
      =  Enable (Subst.subst ve bexp1) (Subst.subst ve choffs) (Subst.subst ve bexp2)

    subst ve (Disable bexp1 bexp2)
      =  Disable (Subst.subst ve bexp1) (Subst.subst ve bexp2)

    subst ve (Interrupt bexp1 bexp2)
      =  Interrupt (Subst.subst ve bexp1) (Subst.subst ve bexp2)

    subst ve (ProcInst pid chans vexps)
      =  ProcInst pid chans (Subst.subst ve vexps)

    subst ve (Hide chids bexp)
      =  Hide chids (Subst.subst ve bexp)

    subst ve (ValueEnv venv bexp)
      =  Subst.subst ve (Subst.subst venv bexp)

    subst ve (StAut stid venv trns)
      =  StAut stid (Map.map (Subst.subst ve) venv) trns


instance Subst Offer
  where
    subst ve (Offer chid choffs)  =  Offer chid (Subst.subst ve choffs)


instance Subst ChanOffer
  where
    subst _  (Quest vid)    =  Quest vid
    subst ve (Exclam vexp)  =  Exclam (Subst.subst ve vexp)


-- ----------------------------------------------------------------------------------------- --
-- variable substition in VExpr


instance Subst VExpr
  where
    subst x = TxsDefs.subst x (Map.empty :: Map.Map FuncId (FuncDef VarId))
    
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
