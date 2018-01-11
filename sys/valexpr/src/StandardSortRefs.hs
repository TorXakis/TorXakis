{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  StandardSortRefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Values for standard sorts
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
module StandardSortRefs
( sortRefBool
, sortRefInt
, sortRefString
, sortRefRegex
, sortRefError
, initialSortRefs
)
where

import           Identifier as Identifer
import           SortDef

-- | Structure containing references for standard 'SortDef's:
--
--   * Error
--
--   * Bool
--
--   * Int
--
--   * String
--
--   * Regex
initialSortRefs :: IdentifierToReference
initialSortRefs = addIdentifier boolIdentifier $
                  addIdentifier intIdentifier $
                  addIdentifier stringIdentifier $
                  addIdentifier regexIdentifier $
                  addIdentifier errorIdentifier
                  Identifer.empty

boolIdentifier :: Identifier
boolIdentifier = Name "Bool"

intIdentifier :: Identifier
intIdentifier = Name "Int"

stringIdentifier :: Identifier
stringIdentifier = Name "String"

regexIdentifier :: Identifier
regexIdentifier = Name "Regex"

errorIdentifier :: Identifier
errorIdentifier = Name "Error"

-- * standard sorts
-- | Reference for Sort Bool
sortRefBool :: TRef SortDef
sortRefBool = getReference boolIdentifier initialSortRefs

-- | Reference for Sort Int
sortRefInt :: TRef SortDef
sortRefInt = getReference intIdentifier initialSortRefs

-- | Reference for Sort String
sortRefString :: TRef SortDef
sortRefString = getReference stringIdentifier initialSortRefs

-- | Reference for Sort Regex
sortRefRegex :: TRef SortDef
sortRefRegex = getReference regexIdentifier initialSortRefs

-- | Reference for Sort Error
sortRefError :: TRef SortDef
sortRefError = getReference errorIdentifier initialSortRefs
