{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEParRemoval
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEParRemoval (
removeParsFromLPE
) where

import qualified Control.Monad       as Monad
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified EnvCore             as IOC
import qualified EnvData
import qualified TxsDefs
import qualified VarId
import           LPETypes
import           LPEBlindSubst

-- Removes the specified parameters an LPE.
-- Occurrences of the parameters in expressions are substituted by their initial values.
removeParsFromLPE :: Set.Set VarId.VarId -> LPE -> IOC.IOC LPE
removeParsFromLPE targetParams lpe
    | targetParams == Set.empty =
        return lpe
    | otherwise = do
        Monad.mapM_ (\p -> IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Removed parameter " ++ Text.unpack (VarId.name p)) ]) (Set.toList targetParams)
        let rho = Map.restrictKeys (lpeInitEqs lpe) targetParams
        newSummands <- Monad.mapM (removeParsFromSummand rho) (Set.toList (lpeSummands lpe))
        return (lpe { lpeInitEqs = Map.withoutKeys (lpeInitEqs lpe) targetParams, lpeSummands = Set.fromList newSummands })
  where
    -- Eliminates parameters from a summand.
    -- Note that channel variables are always fresh, and therefore do not have to be substituted:
    removeParsFromSummand :: Map.Map VarId.VarId TxsDefs.VExpr -> LPESummand -> IOC.IOC LPESummand
    removeParsFromSummand rho contextSummand = do
        guard' <- doConfidentSubst contextSummand rho (lpeSmdGuard contextSummand)
        paramEqs' <- removeParsFromParamEqs contextSummand rho (lpeSmdEqs contextSummand)
        return (contextSummand { lpeSmdGuard = guard', lpeSmdEqs = paramEqs' })
    -- removeParsFromSummand
    
    -- Eliminates parameters from a process instantiation:
    removeParsFromParamEqs :: LPESummand -> Map.Map VarId.VarId TxsDefs.VExpr -> LPEParamEqs -> IOC.IOC LPEParamEqs
    removeParsFromParamEqs contextSummand rho paramEqs = do
        let withoutTargetParams = Map.toList (Map.withoutKeys paramEqs targetParams)
        newAssignments <- Monad.mapM (doConfidentSubst contextSummand rho . snd) withoutTargetParams
        return (Map.fromList (zip (map fst withoutTargetParams) newAssignments))
-- removeParsFromLPE


