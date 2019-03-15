{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  UsedFuncSignatures
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Used Func Signatures in element related functionality.
-----------------------------------------------------------------------------
module TorXakis.FuncSignature.UsedFuncSignatures
( UsedFuncSignatures (..)
  -- dependencies, yet part of interface
, Set.Set
, FuncSignature
)
where
import qualified Data.Set               as Set

import TorXakis.FuncSignature.FuncSignature

-- | Class for Used FuncSignatures
class UsedFuncSignatures a where
    -- | Determine the used FuncSignatures
    usedFuncSignatures :: a -> Set.Set FuncSignature
