{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  UsedProcSignatures
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Used Proc Signatures in element related functionality.
-----------------------------------------------------------------------------
module TorXakis.ProcSignature.UsedProcSignatures
( UsedProcSignatures (..)
  -- dependencies, yet part of interface
, Set.Set
, ProcSignature
)
where
import qualified Data.Set               as Set

import TorXakis.ProcSignature.ProcSignature

-- | Class for Used ProcSignatures
class UsedProcSignatures a where
    -- | Determine the used ProcSignatures
    usedProcSignatures :: a -> Set.Set ProcSignature
