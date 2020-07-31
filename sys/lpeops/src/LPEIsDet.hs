{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEIsDet
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEIsDet (
isDeterministicLPE
) where

import qualified Data.Set as Set
import qualified EnvCore as IOC
import qualified EnvData
import LPETypes
import LPEDeterminism

-- Checks if the given LPE is deterministic.
-- The conclusion is printed to the console, and the input LPE is returned.
isDeterministicLPE :: LPEOperation
isDeterministicLPE lpe _out invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<isdet>" ]
    IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Checking " ++ show (Set.size (lpeSummands lpe)) ++ " summands for possible overlap...") ]
    nonDetSummands <- filterNonDeterministicSummands invariant (lpeSummands lpe)
    if nonDetSummands == Set.empty
    then IOC.putMsgs [ EnvData.TXS_CORE_ANY "Model is deterministic!" ]
    else IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Model may be non-deterministic (found " ++ show (Set.size nonDetSummands) ++ " summands with possible overlap)!") ]
    return (Right lpe)
-- isDeterministicLPE

