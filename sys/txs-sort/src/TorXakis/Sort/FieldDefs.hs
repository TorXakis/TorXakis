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
-- Definitions for fields ('FieldDef') of constructors ('ConstructorDef') of
-- ADTs ('ADTDef'). For unchecked definitions, the type argument sortRef is a
-- 'Name' that refers to a 'Sort'. For checked definitions, sortRef is a 'Sort'.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ExistentialQuantification     #-}
{-# LANGUAGE ScopedTypeVariables     #-}
module TorXakis.Sort.FieldDefs
( -- * Fields
  -- ** Data structure
  FieldDef (..)

  -- ** Collection
, FieldDefs (..)

  -- ** Usage
, emptyFieldDefs
, fieldDefs  

-- * ADT Field Errors
, ADTFieldError (..)
)
where

import           Control.DeepSeq (NFData)
import           Data.Data       (Data)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           TorXakis.Sort.Name       ( Name, HasName, getName
                                          , searchDuplicateNames )
import           TorXakis.Sort.ConvertsTo (ConvertsTo, convertTo)

-- | Data structure for a field definition.
--   For simplicity, anonymous fields are not supported.
data FieldDef sortRef = FieldDef
    { -- | Name of the field
      fieldName :: Name
      -- | Sort of the field
    , sort      :: sortRef
    , metadata  :: Text
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName (FieldDef sr) where
    getName = fieldName

-- | Data structure for a collection of 'FieldDef's.
data FieldDefs sr = FieldDefs { -- | Transform 'FieldDefs' to a list of 'FieldDef's.
                                fDefsToList :: [FieldDef sr]
                                -- | Number of field definitions in a 'FieldDefs'.
                              , nrOfFieldDefs :: Int
                              -- Implementation decision to store value i.s.o.
                              -- calculating it, for performance.
                              -- INVARIANT: length fDefsToList == nrOfFieldDefs
                              }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

emptyFieldDefs :: FieldDefs sr
emptyFieldDefs = FieldDefs [] 0 

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
fieldDefs :: [FieldDef t] -> Either (ADTFieldError t) (FieldDefs t)
fieldDefs fs
    | not $ null nuFieldNames = Left $ FieldNamesNotUnique nuFieldNames
    | otherwise = Right $ FieldDefs fs $ length fs
    where
        nuFieldNames = searchDuplicateNames fs

-- | Type of errors that are raised when it's not possible to build a
--   'FieldDefs' structure via 'fieldDefs' function.
newtype ADTFieldError t = FieldNamesNotUnique [FieldDef t]
    deriving (Eq)

instance Show t => Show (ADTFieldError t) where
    show (FieldNamesNotUnique fDefs) = "Names of following field definitions are not unique: " ++ show fDefs

instance ConvertsTo a a' => ConvertsTo (FieldDef a) (FieldDef a') where
    convertTo (FieldDef n sNm meta) = FieldDef n (convertTo sNm) meta

instance ConvertsTo a a' => ConvertsTo (FieldDefs a) (FieldDefs a') where
    convertTo (FieldDefs fs nr) = FieldDefs (convertTo fs) nr
