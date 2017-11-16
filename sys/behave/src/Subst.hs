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

-- import           Control.Monad.Identity
import           Data.Data
import           Data.Generics.Aliases
-- import           Data.Generics.Schemes
-- import           Data.Map              (Map)

-- TorXakis own modules.

-- | Substitute variables by value expressions contained inside a data
-- structure. Substitution only take place on values of type @ValExpr v@, the
-- other values remain unaltered.
-- gSubst :: (Data d)
--        => VEnv                       -- ^ Mapping from variables to
--                                      -- expressions.
--        -> Map FuncId (FuncDef VarId) -- ^ Mapping of function identifiers to
--                                      -- their definitions.
--        -> d                          -- ^ Input data structure.
--        -> d
-- gSubst ve fis = everywhere' (substT ve fis)
-- gSubst ve fis d = runIdentity $ gfoldl step return (substT ve fis d)
    -- where
      -- step :: Data d => Identity (d -> b) -> d -> Identity b
      -- step cdb d' = cdb <*> pure (gSubst ve fis d')

--      substT ve' fis' = mkT (subst ve' fis')--  `extQ` mkT (substBExpr ve' fis')

      -- substBExpr ve' fis' (ValueEnv venv bexp)   = gSubst ve' fis' (gSubst venv fis' bexp)
      -- substBExpr ve' fis' (StAut stid venv trns) = StAut stid (gSubst ve' fis' venv) trns
      -- substBExpr ve' fis' bexp                   = gmapT (gSubst ve' fis') bexp

-- | Substitution that works on values of type @ValExpr v@ and @BExpr@
-- gSubst :: (Typeable a, Data a) => VEnv -> Map FuncId (FuncDef VarId) -> a -> a
-- gSubst ve fis = substAnyExp `extQ` mkT substBExpr `extQ` mkT (subst ve fis)
--     where
--       substBExpr (ValueEnv venv bexp)   = gSubst ve fis (gSubst venv fis bexp)
--       substBExpr (StAut stid venv trns) = StAut stid (gSubst ve fis venv) trns
--       substBExpr bexp                   = gmapT (gSubst ve fis) bexp

--       substAnyExp e = gmapT (gSubst ve fis) e

import           Data.Map              (Map)
import qualified Data.Map              as Map
import qualified Data.Set              as Set

import           TxsDefs               hiding (subst)

import qualified TxsDefs

-- | Expressions that support a substitution function.
class Typeable e => Subst e where
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

substT :: (Typeable b, Subst e) => TxsDefs.VEnv -> Map FuncId (FuncDef VarId) -> b -> b
substT ve' fis' = mkT (substE ve' fis')

instance Subst BExpr where

    subst ve fis (ValueEnv venv bexp) =
        subst ve fis (subst venv fis bexp)

    subst ve fis (StAut stid venv trns) =
        StAut stid (Map.map (subst ve fis) venv) trns

    subst ve fis e = gmapT (substT ve fis) e
    -- subst _ _ Stop = Stop

    -- subst ve fdefs (ActionPref (ActOffer offs cnrs) bexp) =
    --     ActionPref (ActOffer (Subst.subst ve fdefs offs)
    --                          (Subst.subst ve fdefs cnrs))
    --                (Subst.subst ve fdefs bexp)

    -- subst ve (Guard cnrs bexp)
    --   =  Guard (Subst.subst ve cnrs) (Subst.subst ve bexp)

    -- subst ve (Choice bexps)
    --   =  Choice (Subst.subst ve bexps)

    -- subst ve (Parallel chids bexps)
    --   =  Parallel chids (map (Subst.subst ve) bexps)

    -- subst ve (Enable bexp1 choffs bexp2)
    --   =  Enable (Subst.subst ve bexp1) (Subst.subst ve choffs) (Subst.subst ve bexp2)

    -- subst ve (Disable bexp1 bexp2)
    --   =  Disable (Subst.subst ve bexp1) (Subst.subst ve bexp2)

    -- subst ve (Interrupt bexp1 bexp2)
    --   =  Interrupt (Subst.subst ve bexp1) (Subst.subst ve bexp2)

    -- subst ve (ProcInst pid chans vexps)
    --   =  ProcInst pid chans (Subst.subst ve vexps)

    -- subst ve (Hide chids bexp)
    --   =  Hide chids (Subst.subst ve bexp)

    -- subst ve (ValueEnv venv bexp)
    --   =  Subst.subst ve (Subst.subst venv bexp)

    -- subst ve (StAut stid venv trns)
    --   =  StAut stid (Map.map (Subst.subst ve) venv) trns

instance Subst Offer where
    subst ve fdefs (Offer chid choffs) = Offer chid (subst ve fdefs choffs)

instance Subst ChanOffer where
    subst _  _ (Quest vid)       = Quest vid
    subst ve fdefs (Exclam vexp) = Exclam (subst ve fdefs vexp)

instance Subst VExpr where
    subst = TxsDefs.subst
