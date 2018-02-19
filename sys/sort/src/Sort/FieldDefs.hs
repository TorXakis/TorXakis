{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sort.FieldDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
--                kerem.ispirli@tno.nl
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for fields ('FieldDef') of constructors ('ConstructorDef') of ADTs ('ADTDef').
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Sort.FieldDefs
( -- * Fields
  -- ** Data structure
  FieldDef (..)

  -- ** Collection
, FieldDefs (..)

  -- ** Usage
, fieldDefs

-- * ADT Field Errors
, ADTFieldError (..)
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.List.Unique
import           GHC.Generics     (Generic)

import           Name

-- | Data structure for a field definition.
--   For simplicity, anonymous fields are not supported.
data FieldDef v = FieldDef
    { -- | Name of the field 
      fieldName :: Name
      -- | Sort of the field
    , sort      :: v
      -- TODO: Add meta-data like file name, line nr,
      -- generator, etc. A Text field should be enough.
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Data structure for a collection of 'FieldDef's.
data FieldDefs v = FieldDefs { -- | Transform 'FieldDefs' to a list of 'FieldDef's.
                               fDefsToList :: [FieldDef v]
                               -- | Number of field definitions in a 'FieldDefs'.
                             , nrOfFieldDefs :: Int
                             -- Implementation decision to store value i.s.o.
                             -- calculating it, for performance.
                             -- INVARIANT: length fDefsToList == nrOfFieldDefs
                             }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Smart constructor for 'FieldDefs'.
--
--   Preconditions:
--
--   * Names of 'FieldDef's should be unique
--
--   Given a list of 'FieldDef's,
--
--   * either an error message indicating violations of precondition
--
--   * or a structure containing the field definitions
--
--   is returned.
--
--   Note that the position in the list is relevant as it represents implicit
--   positions of the fields in a constructor.
fieldDefs :: [FieldDef Name] -> Either ADTFieldError (FieldDefs Name)
fieldDefs l
    | not $ null nuFieldNames = let nonUniqDefs = filter ((`elem` nuFieldNames) . fieldName) l
                                in  Left $ FieldNamesNotUnique nonUniqDefs
    | otherwise = Right $ FieldDefs l $ length l
    where
        nuFieldNames = repeated $ map fieldName l

-- | Type of errors that are raised when it's not possible to build a
--   'FieldDefs' structure via 'fieldDefs' function.
data ADTFieldError = FieldNamesNotUnique [FieldDef Name]
                   | EmptyFieldDefs
    deriving (Eq)

instance Show ADTFieldError where
    show (FieldNamesNotUnique fDefs) = "Names of following field definitions are not unique: " ++ show fDefs
    show  EmptyFieldDefs             = "No field definitions provided."
