{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module LPEHelpers
( extractVars
, wrapSteps
, extractSteps 
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Map            as Map
import qualified Data.Set            as Set

import TxsDefs
import qualified TxsUtils

import ProcId 
import VarId
import BehExprDefs

import qualified Data.Text         as T


-- ----------------------------------------------------------------------------------------- --
-- Helpers : 
-- ----------------------------------------------------------------------------------------- --

extractVars :: ActOffer -> [VarId]
extractVars actOffer = let  set = offers actOffer in
                       Set.foldr collect [] set 
    where 
        collect :: Offer -> [VarId] -> [VarId]
        collect Offer{chanoffers = coffers} varIds = foldr extractVarIds [] coffers 

        extractVarIds :: ChanOffer -> [VarId] -> [VarId]
        extractVarIds (Quest varId) varIds  = (varId:varIds)
        extractVarIds _ varIds              = varIds



wrapSteps :: [BExpr] -> BExpr
wrapSteps [bexpr] = bexpr
wrapSteps bexprs = Choice bexprs

extractSteps :: BExpr -> [BExpr]
extractSteps (Choice bexprs) = bexprs
extractSteps bexpr = [bexpr]
