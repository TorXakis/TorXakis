{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sort.ADTDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
--                kerem.ispirli@tno.nl
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for Abstract Data Types ('ADTDef's) and 'Sort's.
--
-- We had to put 'Sort' into this file because of the circular dependency
-- caused by the 'Sort.SortADT' constructor.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Sort.ADTDefs
( -- * 'Sort's of Value Expressions
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

-- * ADT Errors
, ADTError (..)
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.List.Unique
import           Data.List        (intercalate,partition)
import qualified Data.Map.Strict  as Map
import           GHC.Generics     (Generic)

import           Id
import           Ref
import           Name
import           Sort.ConstructorDefs
import           Sort.FieldDefs

-- | Data structure for Abstract Data Type (ADT) definition.
data ADTDef v = ADTDef
    { adtName      :: Name              -- ^ Name of the ADT
    , constructors :: ConstructorDefs v -- ^ Constructor definitions of the ADT
    }
    deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

-- | Data structure for a collection of 'ADTDef's.
newtype ADTDefs = ADTDefs { -- | Transform 'ADTDefs' to a 'Data.Map.Map' from 'Ref' 'ADTDef' to 'ADTDef'.
                            adtDefsToMap :: Map.Map (Ref (ADTDef Sort)) (ADTDef Sort)
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
            -> Either ADTError ADTDefs
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

        fixADTSorts :: ADTDef Name -> ADTDef Sort
        fixADTSorts (ADTDef nm (ConstructorDefs cDfsMap)) = 
            ADTDef nm $ ConstructorDefs $ Map.fromList $ map (fixConstructorSorts nm) $ Map.elems cDfsMap
        fixConstructorSorts :: Name -> ConstructorDef Name -> (Ref (ConstructorDef Sort), ConstructorDef Sort)
        fixConstructorSorts adtNm (ConstructorDef nm (FieldDefs fDfs nr)) =
            let cDef = ConstructorDef nm $ FieldDefs (map (fixFieldSorts adtNm) fDfs) nr
            in  (Ref $ Name.toText nm, cDef)
        fixFieldSorts :: Name -> FieldDef Name -> FieldDef Sort
        fixFieldSorts adtNm (FieldDef nm _) = FieldDef nm (SortADT (Ref $ Name.toText adtNm))

data ADTError = RefsNotFound          [([Ref Name], ADTDef Name)]
              | NamesNotUnique        [ADTDef Name]
              | NonConstructableTypes [Name]
        deriving (Eq)

instance Show ADTError where
    show (RefsNotFound                       []) = ""
    show (RefsNotFound ( (uRfs,reqADTDf) : ts) ) = "ADT(s) " ++ show uRfs
                                                ++ " required by ADT '" ++ (show . adtName) reqADTDf
                                                ++ "' are not defined.\n"
                                                ++ show (RefsNotFound ts)
    show (NamesNotUnique                  aDefs) = "Names of following ADT definitions are not unique: " ++ show aDefs
    show (NonConstructableTypes           names) = "ADTs are not constructable: "
                                                ++ intercalate ", " (map show names)
    
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
-- Sort
-----------------------------------------------------------------------------
-- | The data type that represents 'Sort's for 'ValExpr.ValExpr's.
data Sort = SortError
          | SortBool
          | SortInt
          | SortChar
          | SortString
          | SortRegex
          | SortADT (Ref (ADTDef Sort))
     deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

instance Identifiable Sort where
    getId _ = Nothing

instance Resettable Sort where
    reset = id
