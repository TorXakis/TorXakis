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
-- Definitions for:
--
-- * 'Sort'
--
-- * Abstract Data Type - 'ADTDef'
--
-- * Constructor - 'ConstructorDef'
--
-- * Field - 'FieldDef'
--
-- We had to put everything into one file because of the circular dependency
-- caused by the SortADT constructor.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
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
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.List.Unique
import qualified Data.Map         as Map
import           Data.Text        (Text)
import           GHC.Generics     (Generic)

import           Id
import           Ref

-----------------------------------------------------------------------------
-- Sort
-----------------------------------------------------------------------------
-- | The data type that represents 'Sort's for 'ValExpr.ValExpr's.
data Sort = SortError
          | SortBool
          | SortInt
          | SortChar
          | SortString
          | SortRegex
          | SortADT (Ref ADTDef)
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Identifiable Sort where
    getId _ = Nothing

instance Resettable Sort where
    reset = id

-----------------------------------------------------------------------------
-- Abstract Data Type
-----------------------------------------------------------------------------
-- | Data structure for Abstract Data Type (ADT) definition.
data ADTDef = ADTDef
    { adtName      :: Text       -- ^ Name of the ADT
    , constructors :: ConstructorDefs -- ^ Constructor definitions of the ADT
    }
    deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

-- | Data structure for a collection of 'ADTDef's.
newtype ADTDefs = ADTDefs { -- | Transform 'ADTDefs' to a 'Data.Map.Map' from 'Ref' 'ADTDef' to 'ADTDef'.
                            adtDefsToMap :: Map.Map (Ref ADTDef) ADTDef
                            }
    deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

-- | Smart constructor for 'ADTDefs'.
--
--   Creates an empty 'ADTDefs'.
emptyADTDefs ::  ADTDefs
emptyADTDefs = ADTDefs Map.empty

-- | Smart constructor for 'ADTDefs'.
--
--   Preconditions:
--
--   * 'Ref's to 'ADTDef's should be unique
--
--   * Names of 'ADTDef's should be unique
--
--   * TODO: All references should exist in 'ADTDefs' and given list
--
--   * TODO: Data types should be constructable
--
--   Given a list of tuples of 'Ref' 'ADTDef' and 'ADTDef', and an 'ADTDefs'
--
--   * either an error message indicating violations of preconditions
--
--   * or a structure containing all data types
--
--   is returned.
addADTDefs :: [(Ref ADTDef, ADTDef)]
            -> ADTDefs
            -> Either String ADTDefs
addADTDefs l adfs =
    let adtMap = adtDefsToMap adfs
        nonUniqueRefs  = repeated $ Prelude.map fst l
        nonUniqueNames = repeated $ Prelude.map ( adtName . snd ) l
        -- (constructableTypes,nonConstructableTypes) = 
    in if null nonUniqueRefs && null nonUniqueNames
        then Right $ ADTDefs $ Map.union adtMap $ Map.fromList l
        else let refErr = if not $ null nonUniqueRefs
                        then let nonUniqTuples = filter ((`elem` nonUniqueRefs) . fst) l
                            in  "Refs are not unique: " ++ show nonUniqTuples
                        else ""
                 nameErr = if not $ null nonUniqueNames
                        then let nonUniqTuples = filter ((`elem` nonUniqueNames) . adtName . snd) l
                            in  "Names are not unique: " ++ show nonUniqTuples
                        else ""
             in  Left $ refErr ++ "\n" ++ nameErr

-- type ConstructableADTs = [ADTDef]
-- type InconstructableADTs = [ADTDef]

-- checkConstructable :: (ConstructableADTs,InconstructableADTs)
--                    -> [ADTDef]
--                    -> (ConstructableADTs,InconstructableADTs)
-- checkConstructable result [] = result
-- checkConstructable (cs, ics) (ad:ads)
--     | any (==0) $ Prelude.map (nrOfFieldDefs . fields) $ Map.values $ cDefsToMap $ constructors ad =
--         checkConstructable ((ad:cs), ics) ads
-- checkConstructable (cs, ics) (ad:ads) =
-- checkConstructable (cs, ics) (ad:ads) =

-----------------------------------------------------------------------------
-- Constructor
-----------------------------------------------------------------------------
-- | Data structure for constructor definition.
data ConstructorDef = ConstructorDef { constructorName :: Text -- ^ Name of the constructor
                                     , fields :: FieldDefs          -- ^ Field definitions of the constructor
                                     }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | Data structure for a collection of 'ConstructorDef's.
newtype ConstructorDefs = ConstructorDefs { -- | Transform 'ConstructorDefs' to a 'Data.Map.Map' from 'Ref' 'ConstructorDef' to 'ConstructorDef'.
                                            cDefsToMap :: Map.Map (Ref ConstructorDef) ConstructorDef
                                          }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | Smart constructor for 'ConstructorDefs'.
--
--   Preconditions:
--
--   * 'Ref's to 'ConstructorDef's should be unique
--
--   * Names of 'ConstructorDef's should be unique
--
--   * Names of 'FieldDef's should be unique across all 'ConstructorDef's
--
--   Given a list of tuples of 'Ref' 'ConstructorDef' and 'ConstructorDef',
--
--   * either an error message indicating violations of preconditions
--
--   * or a structure containing the constructor definitions
--
--   is returned.
constructorDefs :: [(Ref ConstructorDef, ConstructorDef)]
                -> Either String ConstructorDefs
constructorDefs l = let nonUniqueRefs  = repeated $ map fst l
                        nonUniqueNames = repeated
                            $ map ( constructorName . snd ) l
                        nonUniqueFieldNames = repeated
                            $ map (fieldName . snd)
                            $ concatMap (fDefsToList . fields . snd) l
                    in if null nonUniqueRefs && null nonUniqueNames && null nonUniqueFieldNames
                           then Right $ ConstructorDefs $ Map.fromList l
                           else let refErr = if not $ null nonUniqueRefs
                                        then let nonUniqTuples = filter ((`elem` nonUniqueRefs) . fst) l
                                            in  "Refs are not unique: " ++ show nonUniqTuples
                                        else ""
                                    nameErr = if not $ null nonUniqueNames
                                        then let nonUniqTuples = filter ((`elem` nonUniqueNames) . constructorName . snd) l
                                            in  "Names are not unique: " ++ show nonUniqTuples
                                        else ""
                                    fNameErr = if not $ null nonUniqueFieldNames
                                        then "Field names are not unique: " ++ show nonUniqueFieldNames
                                        else ""
                        in  Left $ refErr ++ "\n" ++ nameErr ++ "\n" ++ fNameErr

-----------------------------------------------------------------------------
-- Field
-----------------------------------------------------------------------------
-- | Data structure for a field definition.
data FieldDef = FieldDef { fieldName :: Text -- ^ Name of the field
                         , sort      :: Sort      -- ^ Sort of the field
                         }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | Data structure for a collection of 'FieldDef's.
data FieldDefs = FieldDefs  { -- | Transform 'FieldDefs' to a list of tuples of 'Ref' 'FieldDef' and 'FieldDef'.
                              fDefsToList :: [(Ref FieldDef, FieldDef)]
                              -- | Number of field definitions in a 'FieldDefs'.
                            , nrOfFieldDefs :: Int
                            }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Smart constructor for 'FieldDefs'.
--
--   Preconditions:
--
--   * 'Ref's to 'FieldDef's should be unique
--
--   * Names of 'FieldDef's should be unique
--
--   Given a list of tuples of 'Ref' 'FieldDef' and 'FieldDef',
--
--   * either an error message indicating violations of preconditions
--
--   * or a structure containing the field definitions
--
--   is returned.
--
--   Note that the position in the list is relevant as it represents implicit
--   positions of the fields in a constructor.
fieldDefs :: [(Ref FieldDef, FieldDef)] -> Either String FieldDefs
fieldDefs l = let nonUniqueRefs  = repeated $ map fst l
                  nonUniqueNames = repeated $ map ( fieldName . snd ) l
              in if null nonUniqueRefs && null nonUniqueNames
                    then Right $ FieldDefs l $ length l
                    else let refErr =
                                if null nonUniqueRefs
                                    then let nonUniqTuples = filter ((`elem` nonUniqueRefs) . fst) l
                                         in  "Refs are not unique: " ++ show nonUniqTuples
                                    else ""
                             nameErr =
                                if null nonUniqueNames
                                    then let nonUniqTuples = filter ((`elem` nonUniqueNames) . fieldName . snd) l
                                         in  "Names are not unique: " ++ show nonUniqTuples
                                    else ""
                  in  Left $ refErr ++ "\n" ++ nameErr

-- | Curates a list of 'Sort's of every field in a 'FieldDefs'.
sortsOfFieldDefs :: FieldDefs -> [Sort]
sortsOfFieldDefs = map (sort . snd) . fDefsToList
