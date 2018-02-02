{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SortInternal
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
module SortInternal
(
  -- * 'Sort's of Value Expressions
  Sort (..)

  -- * Abstract Data Types
  -- ** Data structure
, ADTDef (..)

  -- ** Collection
, ADTDefs (..)

  -- ** Usage
, emptyADTDefs
, addADTDefs

 -- ** Error messages
 , ADTError (..)
 , toErrorText
 
  -- ** Private methods
, verifyConstructableADTs

  -- * Constructors
  -- ** Data structure
, ConstructorDef (..)

  -- ** Collection
, ConstructorDefs (..)

  -- ** Usage
, constructorDefs

  -- * Fields
  -- ** Data structure
, FieldDef (..)

  -- ** Collection
, FieldDefs (..)

  -- ** Usage
, fieldDefs
, sortsOfFieldDefs
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.List.Unique
import           Data.List        (partition)
import qualified Data.Map.Strict  as Map
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T
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
--   This function can be used to add new 'ADTDef's to an 'emptyADTDefs' or a
--   non-empty 'ADTDefs'.
--
--   Preconditions:
--
--   * 'Ref's to 'ADTDef's should be unique
--
--   * Names of 'ADTDef's should be unique and non-empty
--
--   * All references should exist in 'ADTDefs' or given list
--
--   * All data types should be constructable
--
--   Given a list of tuples of 'Ref' 'ADTDef' and 'ADTDef', and an 'ADTDefs'
--
--   * either an error message indicating some violations of preconditions
--
--   * or a structure containing all data types
--
--   is returned.
addADTDefs :: [(Ref ADTDef, ADTDef)]
            -> ADTDefs
            -> Either (ADTError ADTDef) ADTDefs
addADTDefs l adfs
    | not $ null nuRefs                = let nonUniqTuples = filter ((`elem` nuRefs) . fst) l
                                         in  Left $ RefsNotUnique nonUniqTuples
    | not $ null refsToEmptyNamedADTs  = Left $ EmptyName refsToEmptyNamedADTs
    | not $ null nuNames               = let nonUniqTuples = filter ((`elem` nuNames) . adtName . snd) l
                                         in  Left $ NamesNotUnique nonUniqTuples
    | not $ null unknownRefs           = Left $ RefsNotFound unknownRefs
    | not $ null nonConstructableTypes = let ncADTNames = map (adtName . snd) nonConstructableTypes
                                         in  Left $ NonConstructableTypes ncADTNames
    | otherwise = Right $ ADTDefs $ Map.union adtMap $ Map.fromList l
    where
        adtMap = adtDefsToMap adfs
        nuRefs  = repeated (map fst l ++ Map.keys adtMap)
        refsToEmptyNamedADTs = map fst $ filter (T.null . adtName . snd) l
        nuNames = repeated (map ( adtName . snd ) $ l ++ Map.toList adtMap)
        unknownRefs = filter (not . null . fst)
                      $ map (\(r,adt) -> (getAbsentADTRefs $ fieldSorts adt,(r,adt))) l

        allRefs = map fst l ++ Map.keys adtMap
        fieldSorts :: ADTDef -> [Sort]
        fieldSorts adt = concatMap (sortsOfFieldDefs . fields)
                         $ Map.elems $ cDefsToMap $ constructors adt
        getAbsentADTRefs :: [Sort] -> [Ref ADTDef]
        getAbsentADTRefs [] = []
        getAbsentADTRefs (SortADT r : ss)
            | r `notElem` allRefs = r : getAbsentADTRefs ss
            | otherwise           = getAbsentADTRefs ss
        getAbsentADTRefs (_:ss) = getAbsentADTRefs ss

        nonConstructableTypes = snd $ verifyConstructableADTs (adtMap, l)

data ADTError t = RefsNotUnique         { nonUniqueRefs         :: [(Ref t, t)] }
                | EmptyName             { emptyNamedRfs         :: [Ref t] }
                | NamesNotUnique        { nonUniqueNames        :: [(Ref t, t)] }
                | RefsNotFound          { notFoundRefs          :: [([Ref ADTDef], (Ref ADTDef, ADTDef))] }
                | NonConstructableTypes { nonConstructableNames :: [Text] }
                | SameFieldMultipleCstr { fieldNames            :: [Text] }
        deriving (Eq, Show)


toErrorText :: (Show t) => ADTError t -> Text
toErrorText (RefsNotUnique        tuples) = T.pack "Refs are not unique: " <> T.pack (show tuples)
toErrorText (EmptyName              refs) = T.pack "Names can't be empty: " <> T.pack (show refs)
toErrorText (NamesNotUnique       tuples) = T.pack "Names are not unique: " <> T.pack (show tuples)
toErrorText (RefsNotFound             []) = T.empty
toErrorText (RefsNotFound ((uRfs,(reqADTRf, reqADTDf)):ts)) =
                                            T.pack "ADT(s) " <> T.pack (show uRfs)
                                            <> T.pack " required by ADT '" <> adtName reqADTDf
                                            <> T.pack " - " <> T.pack (show reqADTRf)
                                            <> T.pack "' are not defined.\n"
                                            <> toErrorText (RefsNotFound ts :: ADTError ADTDef)
toErrorText (NonConstructableTypes names) = T.pack "ADTs are not constructable: "
                                            <> T.intercalate (T.pack ", ") names
toErrorText (SameFieldMultipleCstr names) = T.pack "Field names in multiple constructors: "
                                            <> T.intercalate (T.pack ", ") names

-- | Verifies if given list of 'ADTDef's are constructable.
--   Input: A tuple consisting of:
--
--   * 'Map.Map' of Ref's to known constructable 'ADTDef's
--
--   * A list of Ref-ADTDef tuples to be verified
verifyConstructableADTs :: (Map.Map (Ref ADTDef) ADTDef, [(Ref ADTDef, ADTDef)])
                        -> (Map.Map (Ref ADTDef) ADTDef, [(Ref ADTDef, ADTDef)]) 
verifyConstructableADTs (cADTs, uADTs) =
    let (cs,ncs)  = partition
                        (any (allFieldsConstructable cADTs) . Map.elems . cDefsToMap . constructors . snd)
                        uADTs
    in  if null cs
            then (cADTs,uADTs)
            else verifyConstructableADTs
                 (foldl (\accM (r,a) -> Map.insert r a accM) cADTs cs, ncs)

allFieldsConstructable :: Map.Map (Ref ADTDef) ADTDef -> ConstructorDef -> Bool
allFieldsConstructable cADTs cDef = all (isSortConstructable cADTs) $ sortsOfFieldDefs $ fields cDef

isSortConstructable :: Map.Map (Ref ADTDef) ADTDef -> Sort -> Bool
isSortConstructable cADTs (SortADT sortADTRef) = Map.member sortADTRef cADTs
isSortConstructable _ _ = True

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
                -> Either (ADTError ConstructorDef) ConstructorDefs
constructorDefs l
    | not $ null nuRefs         = let nonUniqTuples = filter ((`elem` nuRefs) . fst) l
                                  in  Left $ RefsNotUnique nonUniqTuples
    | not $ null emptyNamedCRfs = Left $ EmptyName emptyNamedCRfs
    | not $ null nuCstrNames    = let nonUniqTuples = filter ((`elem` nuCstrNames) . constructorName . snd) l
                                  in  Left $ NamesNotUnique nonUniqTuples
    | not $ null nuFieldNames   = Left $ SameFieldMultipleCstr nuFieldNames
    | otherwise = Right $ ConstructorDefs $ Map.fromList l
    where
        nuRefs         = repeated $ map fst l
        emptyNamedCRfs = map fst $ filter (T.null . constructorName . snd) l
        nuCstrNames    = repeated $ map ( constructorName . snd ) l
        nuFieldNames   = repeated $ map (fieldName . snd) $ concatMap (fDefsToList . fields . snd) l

-----------------------------------------------------------------------------
-- Field
-----------------------------------------------------------------------------
-- | Data structure for a field definition.
data FieldDef = FieldDef { fieldName :: Text -- ^ Name of the field
                         , sort      :: Sort -- ^ Sort of the field
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
fieldDefs :: [(Ref FieldDef, FieldDef)] -> Either Text FieldDefs
fieldDefs l = let fnonUniqueRefs  = repeated $ map fst l
                  fnonUniqueNames = repeated $ map ( fieldName . snd ) l
              in if null fnonUniqueRefs && null fnonUniqueNames
                    then Right $ FieldDefs l $ length l
                    else let 
                            refErr =
                                if null fnonUniqueRefs
                                    then let nonUniqTuples = filter ((`elem` fnonUniqueRefs) . fst) l
                                         in  "Refs are not unique: " ++ show nonUniqTuples
                                    else ""
                            nameErr =
                                if null fnonUniqueNames
                                    then let nonUniqTuples = filter ((`elem` fnonUniqueNames) . fieldName . snd) l
                                         in  "Names are not unique: " ++ show nonUniqTuples
                                    else ""
                  in  Left $ T.pack $ unlines [refErr, nameErr]

-- | Curates a list of 'Sort's of every field in a 'FieldDefs'.
sortsOfFieldDefs :: FieldDefs -> [Sort]
sortsOfFieldDefs = map (sort . snd) . fDefsToList
