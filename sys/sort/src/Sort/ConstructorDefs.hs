{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sort.ConstructorDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
--                kerem.ispirli@tno.nl
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for constructors ('ConstructorDef') of ADTs ('ADTDef').
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sort.ConstructorDefs
( -- * Constructors
  -- ** Data structure
  ConstructorDef (..)

-- ** Collection
, ConstructorDefs (..)

-- ** Usage
, constructorDefs
, getFieldNames
, getFieldSorts
, getAllFieldSortNames

-- * ADT Constructor Errors
, ADTConstructorError (..)
)
where

import           Control.Arrow
import           Control.DeepSeq
import           Data.Data
import           Data.List           (intercalate)
import qualified Data.HashMap.Strict as Map
import           GHC.Generics        (Generic)

import           Ref
import           Name
import           Sort.ConvertsTo
import           Sort.FieldDefs

-- | Data structure for constructor definition.
data ConstructorDef sortRef = ConstructorDef
                                { constructorName :: Name  -- ^ Name of the constructor
                                , fields :: FieldDefs sortRef -- ^ Field definitions of the constructor
                                }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName (ConstructorDef sr) where
    getName = constructorName

instance Referencable (ConstructorDef sr)

-- | Data structure for a collection of 'ConstructorDef's.
newtype ConstructorDefs sr = ConstructorDefs
                                { -- | Transform 'ConstructorDefs' to a 'Data.HashMap.HashMap' structure from 'Ref' 'ConstructorDef' to 'ConstructorDef'.
                                cDefsToMap :: Map.HashMap (Ref (ConstructorDef sr)) (ConstructorDef sr)
                                }
    deriving (Eq, Read, Show, Generic, NFData, Data)

-- | Smart constructor for 'ConstructorDefs'.
--
--   Preconditions:
--
--   * List of 'ConstructorDef's should be non-empty.
--
--   * Names of 'ConstructorDef's should be unique
--
--   * Names of 'FieldDef's should be unique across all 'ConstructorDef's
--
--   Given a list of 'ConstructorDef's,
--
--   * either an error message indicating violations of preconditions
--
--   * or a 'ConstructorDefs' structure containing the constructor definitions
--
--   is returned.
constructorDefs :: [ConstructorDef Name]
                -> Either ADTConstructorError (ConstructorDefs Name)
constructorDefs [] = Left EmptyConstructorDefs
constructorDefs cs
    | not $ null nuCstrDefs   = Left $ ConstructorNamesNotUnique nuCstrDefs
    | not $ null nuFieldNames = Left $ SameFieldMultipleCstr nuFieldNames
    | otherwise = Right $ ConstructorDefs $ convertTo cs
    where
        nuCstrDefs   = searchDuplicateNames cs
        nuFieldNames = searchDuplicateNames (concatMap (fDefsToList . fields) cs)

-- | Extract 'Name's from 'FieldDef's of given 'ConstructorDef'.
getFieldNames :: ConstructorDef v -> [Name]
getFieldNames = map fieldName . fDefsToList . fields

-- | Extract sorts from 'FieldDef's of given 'ConstructorDef'; whether they ore
--   'Name's or 'Sort's.
getFieldSorts :: ConstructorDef v -> [v]
getFieldSorts = map sort . fDefsToList . fields

-- | Extract 'Name's of sorts from 'FieldDef's of all 'ConstructorDef's in given
--  'ConstructDefs'.
getAllFieldSortNames :: ConstructorDefs Name -> [Name]
getAllFieldSortNames = concatMap getFieldSorts . Map.elems . cDefsToMap

-- | Type of errors that are raised when it's not possible to build a
--   'ConstructorDefs' structure via 'constructorDefs' function.
data ADTConstructorError = ConstructorNamesNotUnique [ConstructorDef Name]
                         | EmptyConstructorDefs
                         | SameFieldMultipleCstr     [FieldDef Name]
    deriving (Eq)

instance Show ADTConstructorError where
    show (ConstructorNamesNotUnique cDefs) = "Names of following constructor definitions are not unique: " ++ show cDefs
    show  EmptyConstructorDefs             = "No constructor definitions provided."
    show (SameFieldMultipleCstr     fDefs) = "Field names in multiple constructors: '"
                                                ++ intercalate ", " (map (show . toText . fieldName) fDefs) ++ "'"

instance ConvertsTo a a' => ConvertsTo (ConstructorDef a) (ConstructorDef a') where
    convertTo (ConstructorDef n fs) = ConstructorDef n (convertTo fs)

instance ConvertsTo a a' => ConvertsTo (ConstructorDefs a) (ConstructorDefs a') where
    convertTo (ConstructorDefs csMap) =
        let tuples = Map.toList csMap
            newTuples = map (convertTo *** convertTo) tuples
        in  ConstructorDefs $ Map.fromList newTuples
