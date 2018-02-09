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

 -- * Error messages
 , ADTError (..)
 , toErrorText
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
import           Name

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
    { adtName      :: Name       -- ^ Name of the ADT
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
--   * Names of 'ADTDef's should be unique
--
--   * All referenced ADTs should exist in 'ADTDefs' or given list
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
addADTDefs :: [ADTDef]
            -> ADTDefs
            -> Either (ADTError ADTDef) ADTDefs
addADTDefs l adfs
    | not $ null nuNames               = let nonUniqTuples = filter ((`elem` nuNames) . Name.toText . adtName) l
                                         in  Left $ NamesNotUnique nonUniqTuples
    | not $ null unknownRefs           = Left $ RefsNotFound unknownRefs
    | not $ null nonConstructableTypes = let ncADTNames = map adtName nonConstructableTypes
                                         in  Left $ NonConstructableTypes ncADTNames
    | otherwise = Right $ ADTDefs $ Map.union adtMap $ Map.fromList $ map (\ad -> (Ref $ Name.toText $ adtName ad, ad)) l
    where
        adtMap = adtDefsToMap adfs
        definedADTs = Map.elems adtMap
        nuNames = repeated allNames
        unknownRefs = filter (not . null . fst)
            $ map (\adt -> (getAbsentADTRefs $ fieldSorts adt, adt)) l

        allNames = map (Name.toText . adtName) l ++ map (Name.toText . adtName) definedADTs
        fieldSorts :: ADTDef -> [Sort]
        fieldSorts adt = concatMap (sortsOfFieldDefs . fields)
                         $ Map.elems $ cDefsToMap $ constructors adt
        getAbsentADTRefs :: [Sort] -> [Ref ADTDef]
        getAbsentADTRefs [] = []
        getAbsentADTRefs (SortADT r : ss)
            | Ref.toText r `notElem` allNames = r : getAbsentADTRefs ss
            | otherwise           = getAbsentADTRefs ss
        getAbsentADTRefs (_:ss) = getAbsentADTRefs ss


        nonConstructableTypes = snd $ verifyConstructableADTs (definedADTs, l)

data ADTError t = RefsNotFound          { notFoundRefs          :: [([Ref ADTDef], ADTDef)] }
                | NamesNotUnique        { nonUniqueNames        :: [t] }
                | NonConstructableTypes { nonConstructableNames :: [Name] }
                | SameFieldMultipleCstr { fieldNames            :: [Name] }
                | EmptyDefs
        deriving (Eq, Show)


toErrorText :: (Show t) => ADTError t -> Text
toErrorText EmptyDefs                     = T.pack "No definitions provided."
toErrorText (NamesNotUnique       tuples) = T.pack "Names are not unique: " <> T.pack (show tuples)
toErrorText (NonConstructableTypes names) = T.pack "ADTs are not constructable: "
                                            <> T.intercalate (T.pack ", ") (map Name.toText names)
toErrorText (SameFieldMultipleCstr names) = T.pack "Field names in multiple constructors: "
                                            <> T.intercalate (T.pack ", ") (map Name.toText names)
toErrorText (RefsNotFound             []) = T.empty
toErrorText (RefsNotFound ( (uRfs,reqADTDf) : ts) ) =
                                            T.pack "ADT(s) " <> T.pack (show uRfs)
                                            <> T.pack " required by ADT '" <> (Name.toText . adtName) reqADTDf
                                            <> T.pack "' are not defined.\n"
                                            <> toErrorText (RefsNotFound ts :: ADTError ADTDef)
                                            
-- | Verifies if given list of 'ADTDef's are constructable.
--
--   Input: A tuple consisting of:
--
--   * A list of known constructable 'ADTDef's
--
--   * A list of 'ADTDef's to be verified
--
--   Output: A tuple consisting of:
--
--   * A list of constructable 'ADTDef's
--
--   * A list of non-constructable 'ADTDef's
verifyConstructableADTs :: ([ADTDef], [ADTDef])
                        -> ([ADTDef], [ADTDef]) 
verifyConstructableADTs (cADTs, uADTs) =
    let (cs,ncs)  = partition
                        (any (allFieldsConstructable cADTs) . Map.elems . cDefsToMap . constructors)
                        uADTs
    in  if null cs
            then (cADTs,uADTs)
            else verifyConstructableADTs (cs ++ cADTs, ncs)

allFieldsConstructable :: [ADTDef] -> ConstructorDef -> Bool
allFieldsConstructable cADTs cDef = all (isSortConstructable cADTs) $ sortsOfFieldDefs $ fields cDef

isSortConstructable :: [ADTDef] -> Sort -> Bool
isSortConstructable cADTs (SortADT sortADTRef) = any (\a -> (Name.toText . adtName) a == Ref.toText sortADTRef) cADTs
isSortConstructable _ _ = True

-----------------------------------------------------------------------------
-- Constructor
-----------------------------------------------------------------------------
-- | Data structure for constructor definition.
data ConstructorDef = ConstructorDef { constructorName :: Name -- ^ Name of the constructor
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
constructorDefs :: [ConstructorDef]
                -> Either (ADTError ConstructorDef) ConstructorDefs
constructorDefs [] = Left EmptyDefs
constructorDefs l
    | not $ null nuCstrNames    = let nonUniqTuples = filter ((`elem` nuCstrNames) . constructorName) l
                                  in  Left $ NamesNotUnique nonUniqTuples
    | not $ null nuFieldNames   = Left $ SameFieldMultipleCstr nuFieldNames
    | otherwise = Right $ ConstructorDefs $ Map.fromList $ map (\cd -> (Ref $ Name.toText $ constructorName cd, cd)) l
    where
        nuCstrNames    = repeated $ map constructorName l
        nuFieldNames   = repeated $ map fieldName $ concatMap (fDefsToList . fields) l

-----------------------------------------------------------------------------
-- Field
-----------------------------------------------------------------------------
-- | Data structure for a field definition.
data FieldDef = FieldDef { fieldName :: Name -- ^ Name of the field
                         , sort      :: Sort -- ^ Sort of the field
                         }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | Data structure for a collection of 'FieldDef's.
data FieldDefs = FieldDefs  { -- | Transform 'FieldDefs' to a list of 'FieldDef's.
                              fDefsToList :: [FieldDef]
                              -- | Number of field definitions in a 'FieldDefs'.
                            , nrOfFieldDefs :: Int
                            }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Smart constructor for 'FieldDefs'.
--
--   Precondition:
--
--   * List of 'FieldDef's should be non-empty.
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
fieldDefs :: [FieldDef] -> Either Text FieldDefs
fieldDefs [] = Left $ T.pack ""
fieldDefs l = let fnonUniqueNames = repeated $ map fieldName l
              in if null fnonUniqueNames
                    then Right $ FieldDefs l $ length l
                    else
                        let nonUniqTuples = filter ((`elem` fnonUniqueNames) . fieldName) l
                            nameErr = "Names are not unique: " ++ show nonUniqTuples
                        in  Left $ T.pack nameErr

-- | Creates a list of 'Sort's of every field in a 'FieldDefs'.
sortsOfFieldDefs :: FieldDefs -> [Sort]
sortsOfFieldDefs = map sort . fDefsToList
