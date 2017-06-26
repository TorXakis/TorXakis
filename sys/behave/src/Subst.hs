{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
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

import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import TxsDefs
import TxsUtils

-- ----------------------------------------------------------------------------------------- --
-- Substitution


class Subst e
  where
    subst :: VEnv -> e -> e


instance (Ord e,Subst e) => Subst [e]
  where
    subst ve =  map (subst ve)


instance (Ord e,Subst e) => Subst (Set.Set e)
  where
    subst ve =  Set.map (subst ve)


-- ----------------------------------------------------------------------------------------- --
-- variable substitution in BExpr


instance Subst BExpr
  where

    subst ve Stop
      =  Stop

    subst ve (ActionPref (ActOffer offs cnrs)  bexp)
      =  ActionPref (ActOffer (subst ve offs) (subst ve cnrs)) (subst ve bexp)

    subst ve (Guard cnrs bexp)
      =  Guard (subst ve cnrs) (subst ve bexp)

    subst ve (Choice bexps)
      =  Choice (subst ve bexps)

    subst ve (Parallel chids bexps)
      =  Parallel chids (map (subst ve) bexps)
    
    subst ve (Enable bexp1 choffs bexp2)
      =  Enable (subst ve bexp1) (subst ve choffs) (subst ve bexp2)

    subst ve (Disable bexp1 bexp2)
      =  Disable (subst ve bexp1) (subst ve bexp2)

    subst ve (Interrupt bexp1 bexp2)
      =  Interrupt (subst ve bexp1) (subst ve bexp2)

    subst ve (ProcInst pid chans vexps)
      =  ProcInst pid chans (subst ve vexps)

    subst ve (Hide chids bexp)
      =  Hide chids (subst ve bexp)

    subst ve (ValueEnv venv bexp)
      =  subst ve (subst venv bexp)

    subst ve (StAut stid venv trns)
      =  StAut stid (Map.map (subst ve) venv) trns


instance Subst Offer
  where
    subst ve (Offer chid choffs)  =  Offer chid (subst ve choffs)


instance Subst ChanOffer
  where
    subst ve (Quest vid)    =  Quest vid
    subst ve (Exclam vexp)  =  Exclam (subst ve vexp)


-- ----------------------------------------------------------------------------------------- --
-- variable substition in VExpr


instance Subst VExpr
  where
    subst = partSubst
    
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

