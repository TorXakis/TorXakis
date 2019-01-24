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
module TorXakis.FreeVars
( FreeVars (..)
)
where
import qualified Data.Set               as Set


import TorXakis.Name
import TorXakis.VarDef

-- | Class for Free Variables
class FreeVars a where
    -- | Determine the free variables
    freeVars :: a -> Set.Set (RefByName VarDef)
    -- | Is expression closed?
    -- A closed expression has no free variables.
    isClosed :: a -> Bool
    isClosed = Set.null . freeVars