{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Sort.SortContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Sort: all defined sorts and necessary other definitions
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.SortContext
(-- * Sort Context
  SortContext (..)
  -- dependencies, yet part of interface
, Error
, Sort
, RefByName
, ADTDef
)
where
import           TorXakis.Error                 (Error)
import           TorXakis.Name
import           TorXakis.SortADT               (Sort(..), ADTDef(adtName))

-- | A Sort Context instance 
-- contains all definitions to work with sorts and references thereof.
class SortContext c where
    -- | Is the provided sort a member of the context?
    memberSort :: Sort -> c -> Bool
    -- | All Sort elements in the context
    elemsSort :: c -> [Sort]
    elemsSort ctx =   SortBool
                    : SortInt
                    : SortChar
                    : SortString
                    : SortRegex
                    : map (SortADT . RefByName . adtName) (elemsADT ctx)

    -- | Points the provided name to an ADTDef in the context?
    memberADT :: Name -> c -> Bool
    -- | lookup ADTDef using the provided name
    lookupADT :: Name -> c -> Maybe ADTDef
    -- | All ADTDef elements in the context
    elemsADT :: c -> [ADTDef]
    -- | Add adt definitions to sort context.
    --   A sort context is returned when the following constraints are satisfied:
    --
    --   * The references of ADTDef are unique
    --
    --   * All references are known
    --
    --   * All ADTs are constructable
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addADTs :: [ADTDef] -> c -> Either Error c


