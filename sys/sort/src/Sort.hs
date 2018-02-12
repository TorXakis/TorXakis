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

  -- * Abstract Data Types
  -- ** Data structure
, ADTDef (..)

  -- ** Collection
, ADTDefs

  -- ** Usage
, adtDefsToMap
, emptyADTDefs
, addADTDefs

  -- * Constructors
  -- ** Data structure
, ConstructorDef (..)

  -- ** Collection
, ConstructorDefs

  -- ** Usage
, cDefsToMap
, constructorDefs

  -- * Fields
  -- ** Data structure
, FieldDef (..)

  -- ** Collection
, FieldDefs

  -- ** Usage
, fDefsToList
, fieldDefs
, nrOfFieldDefs
, sortsOfFieldDefs

 -- * Error messages
 , ADTError
 , toErrorText
)
where

import Sort.Internal