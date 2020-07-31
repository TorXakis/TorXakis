{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEInfo
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEInfo (
lpeInfo
) where

import qualified Data.Set            as Set
import qualified EnvCore             as IOC
import qualified EnvData
import           LPETypes

-- LPE rewrite method.
-- Eliminates parameters that always have the same value from an LPE.
lpeInfo :: LPEOperation
lpeInfo lpe _out _invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<info>>" ]
    IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Parameter count: " ++ show (Set.size (lpeParams lpe))) ]
    IOC.putMsgs [ EnvData.TXS_CORE_ANY ("Summand count: " ++ show (Set.size (lpeSummands lpe))) ]
    return (Right lpe)
-- lpeInfo

