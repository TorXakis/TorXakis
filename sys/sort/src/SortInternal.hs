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
data Sort v = SortError
            | SortBool
            | SortInt
            | SortChar
            | SortString
            | SortRegex
            | SortADT (Ref v)
     deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

instance Identifiable (Sort v) where
    getId _ = Nothing

instance Resettable (Sort v) where
    reset = id

-----------------------------------------------------------------------------
-- Abstract Data Type
-----------------------------------------------------------------------------
-- | Data structure for Abstract Data Type (ADT) definition.
data ADTDef v = ADTDef
    { adtName      :: Name              -- ^ Name of the ADT
    , constructors :: ConstructorDefs v -- ^ Constructor definitions of the ADT
    }
    deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

-- | Data structure for a collection of 'ADTDef's.
newtype ADTDefs = ADTDefs { -- | Transform 'ADTDefs' to a 'Data.Map.Map' from 'Ref' 'ADTDef' to 'ADTDef'.
                            adtDefsToMap :: Map.Map (Ref (ADTDef (Sort ADTDefs))) (ADTDef (Sort ADTDefs))
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
addADTDefs :: [ADTDef Name]
            -> ADTDefs
            -> Either (ADTError (ADTDef Name)) ADTDefs
addADTDefs l adfs
    | not $ null nuNames               = let nonUniqDefs = filter ((`elem` nuNames) . adtName) l
                                         in  Left $ NamesNotUnique nonUniqDefs
    | not $ null unknownRefs           = Left $ RefsNotFound unknownRefs
    | not $ null nonConstructableTypes = let ncADTNames = map adtName nonConstructableTypes
                                         in  Left $ NonConstructableTypes ncADTNames
    | otherwise =
        Right $ ADTDefs
        $ Map.union adtMap
        $ Map.fromList $ map ((\ ad -> (Ref $ Name.toText $ adtName ad, ad)) . fixADTSorts) l
    where
        adtMap = adtDefsToMap adfs
        definedADTs = Map.elems adtMap
        nuNames = repeated allNames
        unknownRefs = filter (not . null . fst)
            $ map (\adt -> (getAbsentADTRefs $ fieldSorts adt, adt)) l

        definedNames = map adtName definedADTs
        allNames = map adtName l ++ definedNames
        fieldSorts :: ADTDef Name -> [Name]
        fieldSorts adt = concatMap (sortsOfFieldDefs . fields)
                         $ Map.elems $ cDefsToMap $ constructors adt
        getAbsentADTRefs :: [Name] -> [Ref Name]
        getAbsentADTRefs [] = []
        getAbsentADTRefs (nm : nms)
            | nm `notElem` allNames = (Ref $ Name.toText nm) : getAbsentADTRefs nms
            | otherwise             = getAbsentADTRefs nms
        nonConstructableTypes = snd $ verifyConstructableADTs (definedNames, l)

        fixADTSorts :: ADTDef Name -> ADTDef (Sort ADTDefs)
        fixADTSorts (ADTDef nm (ConstructorDefs cDfsMap)) = 
            ADTDef nm $ ConstructorDefs $ Map.fromList $ map (fixConstructorSorts nm) $ Map.elems cDfsMap
        fixConstructorSorts :: Name -> ConstructorDef Name -> (Ref (ConstructorDef (Sort ADTDefs)), ConstructorDef (Sort ADTDefs))
        fixConstructorSorts adtNm (ConstructorDef nm (FieldDefs fDfs nr)) =
            let cDef = ConstructorDef nm $ FieldDefs (map (fixFieldSorts adtNm) fDfs) nr
            in  (Ref $ Name.toText nm, cDef)
        fixFieldSorts :: Name -> FieldDef Name -> FieldDef (Sort ADTDefs)
        fixFieldSorts adtNm (FieldDef nm _) = FieldDef nm (SortADT (Ref $ Name.toText adtNm))

data ADTError t = RefsNotFound          { notFoundRefs          :: [([Ref Name], ADTDef Name)] }
                | NamesNotUnique        { nonUniqueNamedDefs    :: [t] }
                | NonConstructableTypes { nonConstructableNames :: [Name] }
                | SameFieldMultipleCstr { fieldNames            :: [Name] }
                | EmptyDefs
        deriving (Eq,Show)

toErrorText :: (Show t) => ADTError t -> Text
toErrorText EmptyDefs                     = T.pack "No definitions provided."
toErrorText (NamesNotUnique         defs) = T.pack "Names of following definitions are not unique: " <> T.pack (show defs)
toErrorText (NonConstructableTypes names) = T.pack "ADTs are not constructable: "
                                            <> T.intercalate (T.pack ", ") (map Name.toText names)
toErrorText (SameFieldMultipleCstr names) = T.pack "Field names in multiple constructors: "
                                            <> T.intercalate (T.pack ", ") (map Name.toText names)
toErrorText (RefsNotFound             []) = T.empty
toErrorText (RefsNotFound ( (uRfs,reqADTDf) : ts) ) =
                                            T.pack "ADT(s) " <> T.pack (show uRfs)
                                            <> T.pack " required by ADT '" <> (Name.toText . adtName) reqADTDf
                                            <> T.pack "' are not defined.\n"
                                            <> toErrorText (RefsNotFound ts :: ADTError (ADTDef Name))
                                            
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
verifyConstructableADTs :: ([Name], [ADTDef Name])
                        -> ([Name], [ADTDef Name]) 
verifyConstructableADTs (cADTNms, uADTDfs) =
    let (cs,ncs)  = partition
                        (any (allFieldsConstructable cADTNms) . Map.elems . cDefsToMap . constructors)
                        uADTDfs
    in  if null cs
            then (cADTNms,uADTDfs)
            else verifyConstructableADTs (map adtName cs ++ cADTNms, ncs)

allFieldsConstructable :: [Name] -> ConstructorDef Name -> Bool
allFieldsConstructable cADTNms cDef = all (isSortConstructable cADTNms) $ sortsOfFieldDefs $ fields cDef
-- TODO: Here we need to handle pre-defined sorts like Int and Bool....


isSortConstructable :: [Name] -> Name -> Bool
isSortConstructable cADTNms adtNm = adtNm `elem` cADTNms

-----------------------------------------------------------------------------
-- Constructor
-----------------------------------------------------------------------------
-- | Data structure for constructor definition.
data ConstructorDef v = ConstructorDef { constructorName :: Name -- ^ Name of the constructor
                                       , fields :: FieldDefs v     -- ^ Field definitions of the constructor
                                       }
    deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

-- | Data structure for a collection of 'ConstructorDef's.
newtype ConstructorDefs v = ConstructorDefs { -- | Transform 'ConstructorDefs' to a 'Data.Map.Map' from 'Ref' 'ConstructorDef' to 'ConstructorDef'.
                                              cDefsToMap :: Map.Map (Ref (ConstructorDef v)) (ConstructorDef v)
                                            }
    deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

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
constructorDefs :: [ConstructorDef v]
                -> Either (ADTError (ConstructorDef v)) (ConstructorDefs v)
constructorDefs [] = Left EmptyDefs
constructorDefs l
    | not $ null nuCstrNames    = let nonUniqDefs = filter ((`elem` nuCstrNames) . constructorName) l
                                  in  Left $ NamesNotUnique nonUniqDefs
    | not $ null nuFieldNames   = Left $ SameFieldMultipleCstr nuFieldNames
    | otherwise = Right $ ConstructorDefs $ Map.fromList $ map (\cd -> (Ref $ Name.toText $ constructorName cd, cd)) l
    where
        nuCstrNames    = repeated $ map constructorName l
        nuFieldNames   = repeated $ map fieldName $ concatMap (fDefsToList . fields) l

-----------------------------------------------------------------------------
-- Field
-----------------------------------------------------------------------------
-- | Data structure for a field definition.
data FieldDef v = FieldDef { fieldName :: Name  -- ^ Name of the field
                           , sort      :: v     -- ^ Sort of the field
                           }
    deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

-- | Data structure for a collection of 'FieldDef's.
data FieldDefs v = FieldDefs { -- | Transform 'FieldDefs' to a list of 'FieldDef's.
                               fDefsToList :: [FieldDef v]
                               -- | Number of field definitions in a 'FieldDefs'.
                             , nrOfFieldDefs :: Int
                             }
    deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

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
fieldDefs :: [FieldDef Name] -> Either (ADTError (FieldDef Name)) (FieldDefs Name)
fieldDefs [] = Left EmptyDefs
fieldDefs l
    | not $ null nuFieldNames = let nonUniqDefs = filter ((`elem` nuFieldNames) . fieldName) l
                                in  Left $ NamesNotUnique nonUniqDefs
    | otherwise = Right $ FieldDefs l $ length l
    where
        nuFieldNames = repeated $ map fieldName l

-- | Creates a list of 'Sort's of every field in a 'FieldDefs'.
sortsOfFieldDefs :: FieldDefs v -> [v]
sortsOfFieldDefs = map sort . fDefsToList
