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
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for 'Sort' and Abstract Data Types.
--
-- We have to put 'Sort' and Abstract Data Types into one file
-- because of the circular dependency caused by the 'Sort.SortADT' constructor.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.Sort.Sort
( -- * Sort
  Sort (..)
  -- ** Has Sort
, HasSort (..)
  -- * Abstract Data Types
  -- ** Field Definition
, FieldDef (..)
  -- ** Constructor Definition
, ConstructorDef
, constructorName
, memberField
, lookupField
, positionField
, elemsField
, mkConstructorDef
  -- ** Abstract Data Type Definition
, ADTDef
, adtName
, memberConstructor
, lookupConstructor
, elemsConstructor
, mkADTDef
  -- * Used Sorts in element
, UsedSorts (..)
  -- dependencies, yet part of interface
, Set.Set
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Hashable       (Hashable(hashWithSalt))
import           Data.List           (elemIndex, find)
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.Error
import           TorXakis.Name
import qualified TorXakis.NameMap
-----------------------------------------------------------------------------
-- Sort
-----------------------------------------------------------------------------
-- | The data type that represents 'Sort'.
data Sort = SortBool
          | SortInt
--        | SortChar
          | SortString
          | SortADT (RefByName ADTDef)
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)
-- If we want to make Sort package more flexible, we can use SortPrim "Int" & SortADT "WhatEver".

instance Hashable Sort where
    s `hashWithSalt` SortBool    = s `hashWithSalt` T.pack "Bool"
    s `hashWithSalt` SortInt     = s `hashWithSalt` T.pack "Int"
--  s `hashWithSalt` SortChar    = s `hashWithSalt` T.pack "Char"
    s `hashWithSalt` SortString  = s `hashWithSalt` T.pack "String"
    s `hashWithSalt` (SortADT r) = s `hashWithSalt` r  -- ADT name differs from predefined names

-- | Enables 'Sort's of entities to be accessed in a common way.
class HasSort c a where
    getSort :: c -> a -> Sort

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

instance HasSort c FieldDef where
    getSort _ = sort

-- | Constructor Definition.
data ConstructorDef = ConstructorDef
                        { -- | Name of the constructor
                          constructorName :: Name
                          -- | Field definitions of the constructor
                          --   Note that the position in the list is relevant as it represents implicitly
                          --   the position of the fields in a constructor.
                          --   In other words, fields are referred to by their position among others to speed up (de)serialization.
                        , fields :: [FieldDef]
                        }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ConstructorDef where
    getName = constructorName

-- | Smart constructor for 'TorXakis.SortADT.ConstructorDef'.
--   A 'TorXakis.SortADT.ConstructorDef' is returned when the following constraint is satisfied:
--
--   * All field names are unique
--
--   Otherwise an error is returned. The error reflects the violations of the aforementioned constraint.
mkConstructorDef :: Name -> [FieldDef] -> Either Error ConstructorDef
mkConstructorDef n fs
    | null nuFields     = Right $ ConstructorDef n fs
    | otherwise         = Left $ Error ("Non unique field names: " ++ show nuFields)
    where
        nuFields :: [FieldDef]
        nuFields = repeatedByName fs

-- | Refers the provided name to a FieldDef in the given ConstructorDef?
memberField :: Name -> ConstructorDef -> Bool
memberField n c = n `elem` map fieldName (fields c)

-- | position Field
positionField :: Name -> ConstructorDef -> Maybe Int
positionField n c = n `elemIndex` map fieldName (fields c)

-- | lookup Field
lookupField :: Name -> ConstructorDef -> Maybe FieldDef
lookupField n = find (\f -> n == fieldName f) . fields

-- | All FieldDefs of given ConstructorDef in order.
elemsField :: ConstructorDef -> [FieldDef]
elemsField = fields

-- | Data structure for Abstract Data Type (ADT) definition.
data ADTDef = ADTDef
    { -- | Name of the ADT
      adtName      :: Name
      -- | Constructor definitions of the ADT
    , constructors :: TorXakis.NameMap.NameMap ConstructorDef
    }
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

instance HasName ADTDef where
    getName = adtName


-- | Smart constructor for 'TorXakis.SortADT.ADTDef'.
--   An 'TorXakis.SortADT.ADTDef' is returned when the following constraints are satisfied:
--
--   * List of 'TorXakis.SortADT.ConstructorDef's is non-empty.
--
--   * Names of 'TorXakis.SortADT.ConstructorDef's are unique
--
--   * Names of 'TorXakis.SortADT.FieldDef's are unique across all 'TorXakis.SortADT.ConstructorDef's
--
--   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
mkADTDef :: Name -> [ConstructorDef] -> Either Error ADTDef
mkADTDef _ [] = Left $ Error "Empty Constructor List"
mkADTDef m cs
    | not $ null nuCstrDefs                 = Left $ Error ("Non-unique constructor definitions: " ++ show nuCstrDefs)
    | not $ null nuFields                   = Left $ Error ("Non-unique field definitions: " ++ show nuFields)
    | otherwise                             = Right $ ADTDef m (TorXakis.NameMap.toNameMap cs)
    where
        nuCstrDefs :: [ConstructorDef]
        nuCstrDefs   = repeatedByName cs

        allFields :: [FieldDef]
        allFields = concatMap fields cs

        nuFields :: [FieldDef]
        nuFields = repeatedByName allFields

-- | Refers the provided ConstructorDef name to a ConstructorDef in the given ADTDef?
memberConstructor :: Name -> ADTDef -> Bool
memberConstructor r a = TorXakis.NameMap.member r (constructors a)

-- | lookup ConstructorDef
lookupConstructor :: Name -> ADTDef -> Maybe ConstructorDef
lookupConstructor r a = TorXakis.NameMap.lookup r (constructors a)

-- | All ConstructorDefs of given ADTDef
elemsConstructor :: ADTDef -> [ConstructorDef]
elemsConstructor = TorXakis.NameMap.elems . constructors

-- | Class for Used Sorts
class UsedSorts c a where
    -- | Determine the used sorts
    usedSorts :: c -> a -> Set.Set Sort

instance UsedSorts c a => UsedSorts c [a] where
    usedSorts ctx = Set.unions . map (usedSorts ctx)

instance UsedSorts c a => UsedSorts c (Set.Set a) where
    usedSorts ctx = Set.unions . map (usedSorts ctx) . Set.toList

instance UsedSorts c FieldDef where
    usedSorts _ = Set.singleton . sort

instance UsedSorts c ConstructorDef where
    usedSorts ctx c = usedSorts ctx (elemsField c)

instance UsedSorts c ADTDef where
    usedSorts ctx a = usedSorts ctx (elemsConstructor a)

-- | Used Names instance
instance UsedNames FieldDef where
    usedNames = Set.singleton . fieldName

instance UsedNames ConstructorDef where
    usedNames c = Set.unions [usedNames (constructorName c), usedNames (elemsField c)]

instance UsedNames ADTDef where
    usedNames a = Set.unions [usedNames (adtName a), usedNames (elemsConstructor a)]
