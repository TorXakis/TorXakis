{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sort
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
--                kerem.ispirli@tno.nl
-- Stability   :  experimental
-- Portability :  portable
--
-- Interface file for Sort.
-----------------------------------------------------------------------------
module Sort
(
  -- * 'Sort's of Value Expressions
  Sort (..)

  -- * Type-safe reference
, Ref (toName)

-- * Abstract Data Types
  -- ** Data structure
, ADTDef (..)

  -- ** Collection
, ADTDefs

  -- ** Usage
, adtDefsToMap
, emptyADTDefs
, addADTDefs
, mergeADTDefs

  -- * Constructors
  -- ** Data structure
, ConstructorDef (..)

  -- ** Collection
, ConstructorDefs

  -- ** Usage
, cDefsToMap
, constructorDefs
, getFieldSorts
-- , getAllFieldSortNames

  -- * Fields
  -- ** Data structure
, FieldDef (..)

  -- ** Collection
, FieldDefs

  -- ** Usage
, fDefsToList
, fieldDefs
, nrOfFieldDefs

 -- * Error messages
 , ADTError
 , ADTConstructorError
 , ADTFieldError
)
where

import Ref
import Sort.ADTDefs
import Sort.ConstructorDefs
import Sort.FieldDefs