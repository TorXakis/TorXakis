{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  UsedNames
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
--                Damian Nadales <damian.nadalesagut@tno.nl>
--                Kerem Ispirli <kerem.ispirli@tno.nl>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides the interface for UsedNames in an entity.
-----------------------------------------------------------------------------
module TorXakis.Name.UsedNames
( 
-- * Used Names in Entity
  UsedNames(..)
)
where
import qualified Data.Set       as Set

import           TorXakis.Name.Name

-- | Class for Used Names
class UsedNames a where
    -- | Determine the used names
    usedNames :: a -> Set.Set Name

instance UsedNames a => UsedNames [a] where
    usedNames = Set.unions . map usedNames

instance UsedNames a => UsedNames (Set.Set a) where
    usedNames = Set.unions . map usedNames . Set.toList

instance UsedNames Name where
    usedNames = Set.singleton

