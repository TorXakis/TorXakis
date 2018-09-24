{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SortADT
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for 'Sort' and Abstract Data Types.
--
-- We have to put 'Sort' and Abstract Data Types into one file 
-- because of the circular dependency caused by the 'Sort.SortADT' constructor.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.SortADT
( -- * Sort
  Sort (..)
  -- ** Has Sort
, HasSort (..)

  -- * Abstract Data Types
  -- ** Field Definition
, FieldDef (..)
  -- ** Constructor Definition
, ConstructorDefView(..)
, ConstructorDef
, viewConstructorDef
, mkConstructorDef
  -- ** Abstract Data Type Definition
, ADTDefView (..)
, ADTDef
, viewADTDef
, mkADTDef
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Hashable       (Hashable(hashWithSalt))
import qualified Data.HashMap        as Map
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.Error      (MinError(MinError))
import           TorXakis.Name       (Name, toText, repeatedByName, HasName, getName, RefByName ( toName ), toMapByName)

-----------------------------------------------------------------------------
-- Sort
-----------------------------------------------------------------------------
-- | The data type that represents 'Sort'.
data Sort = SortBool
          | SortInt
          | SortChar
          | SortString
          | SortRegex
          | SortADT (RefByName ADTDef)
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)
-- If we want to make Sort package more flexible, we can use SortPrim "Int" & SortADT "WhatEver".

instance Hashable Sort where
    hashWithSalt s SortBool    = s `hashWithSalt` T.pack "Bool"
    hashWithSalt s SortInt     = s `hashWithSalt` T.pack "Int"
    hashWithSalt s SortChar    = s `hashWithSalt` T.pack "Char"
    hashWithSalt s SortString  = s `hashWithSalt` T.pack "String"
    hashWithSalt s SortRegex   = s `hashWithSalt` T.pack "Regex"
    hashWithSalt s (SortADT r) = s `hashWithSalt` ( T.pack "A" <> (toText . toName) r )

-- | Enables 'Sort's of entities to be accessed in a common way.
class HasSort a where
    getSort :: a -> Sort

-- | Data structure for a field definition.
data FieldDef = FieldDef
    { -- | Name of the field 
      fieldName :: Name
      -- | Sort of the field
    , sort      :: Sort
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName FieldDef where
    getName = fieldName

instance HasSort FieldDef where
    getSort = sort
    
-- | The public view of 'ConstructorDef'.
data ConstructorDefView = ConstructorDefView
                        { -- | Name of the constructor
                          constructorName :: Name
                          -- | Field definitions of the constructor
                          --   Note that the position in the list is relevant as it represents implicitly
                          --   the position of the fields in a constructor.
                        , fields :: [FieldDef]
                        }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ConstructorDefView where
    getName = constructorName

-- | ConstructorDefinition
newtype ConstructorDef = ConstructorDef { -- | View on constructor definition.
                                          viewConstructorDef :: ConstructorDefView
                                        }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ConstructorDef where
    getName = getName . viewConstructorDef

-- | Smart constructor for ConstructorDef
mkConstructorDef :: Name -> [FieldDef] -> Either MinError ConstructorDef
mkConstructorDef n fs
    | not $ null nuFieldNames = Left $ MinError (T.pack ("Non unique field names: " ++ show nuFieldNames))
    | otherwise               = Right $ ConstructorDef $ ConstructorDefView n fs
    where
        nuFieldNames :: [FieldDef]
        nuFieldNames = repeatedByName fs

-- | The public view of 'ADTDef'
data ADTDefView = ADTDefView
    { -- | Name of the ADT
      adtName      :: Name
      -- | Constructor definitions of the ADT
    , constructors :: Map.Map (RefByName ConstructorDef) ConstructorDef
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ADTDefView where
    getName = adtName

-- | Data structure for Abstract Data Type (ADT) definition.
newtype ADTDef = ADTDef { -- | View on 'ADTDEf'
                          viewADTDef :: ADTDefView
                        }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ADTDef where
    getName = getName . viewADTDef

-- | An ADTDef is returned when the following constraints are satisfied:
--
--   * List of 'ConstructorDef's is non-empty.
--
--   * Names of 'ConstructorDef's are unique
--
--   * Names of 'FieldDef's are unique across all 'ConstructorDef's
--
--   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
mkADTDef :: Name -> [ConstructorDef] -> Either MinError ADTDef
mkADTDef _ [] = Left $ MinError (T.pack "Empty Constructor List")
mkADTDef m cs
    | not $ null nuCstrDefs   = Left $ MinError (T.pack ("Non-unique constructor definitions" ++ show nuCstrDefs))
    | not $ null nuFieldNames = Left $ MinError (T.pack ("Non-unique field definitions" ++ show nuFieldNames))
    | otherwise               = Right $ ADTDef $ ADTDefView m (toMapByName cs)
    where
        nuCstrDefs :: [ConstructorDef]
        nuCstrDefs   = repeatedByName cs
        
        nuFieldNames :: [FieldDef]
        nuFieldNames = repeatedByName (concatMap (fields . viewConstructorDef) cs)
