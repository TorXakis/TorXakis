{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeVars
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Free Variables in Value Expression related functionality.
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.FreeVars
( FreeVars (..)
)
where
import qualified Data.Set               as Set

import           TorXakis.VarDef

-- | Class for Free Variables
class VarDef v => FreeVars a v where
    -- | Determine the free variables
    freeVars :: a v -> Set.Set v
    -- | Is expression closed?
    -- A closed expression has no free variables.
    isClosed :: a v -> Bool
    isClosed = Set.null . freeVars