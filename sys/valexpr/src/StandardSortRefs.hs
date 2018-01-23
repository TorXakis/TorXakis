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
( initialSortRefs
, boolIdentifier
, intIdentifier
, stringIdentifier
, regexIdentifier
, errorIdentifier
)
where

import           Identifier

-- | Structure containing identifiers for standard 'SortDef's:
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
                  Identifier.empty

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
