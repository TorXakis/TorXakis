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
import qualified Data.Text        as T
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
        let fixADTSorts :: ADTDef Name -> ADTDef Sort
            fixADTSorts (ADTDef nm (ConstructorDefs cDfsMap)) = 
                ADTDef nm $ ConstructorDefs $ Map.fromList $ map fixConstructorSorts $ Map.elems cDfsMap
            fixConstructorSorts :: ConstructorDef Name -> (Ref (ConstructorDef Sort), ConstructorDef Sort)
            fixConstructorSorts (ConstructorDef nm (FieldDefs fDfs nr)) =
                let cDef = ConstructorDef nm $ FieldDefs (map fixFieldSorts fDfs) nr
                in  (Ref $ Name.toText nm, cDef)
            fixFieldSorts :: FieldDef Name -> FieldDef Sort
            fixFieldSorts (FieldDef nm s) = FieldDef nm $ (read . T.unpack . Name.toText) s
        in  Right $ ADTDefs
            $ Map.union adtMap
            $ Map.fromList $ map ((\ ad -> (Ref $ Name.toText $ adtName ad, ad)) . fixADTSorts) l
    where
        adtMap = adtDefsToMap adfs
        definedADTs = Map.elems adtMap
        nuNames = repeated allNames
        unknownRefs = filter (not . null . fst)
            $ map (\adt -> (getAbsentADTRefs $ fieldSorts adt, adt)) l
            where fieldSorts adt = concatMap (sortsOfFieldDefs . fields) $ (Map.elems . cDefsToMap . constructors) adt

        allNames = map adtName definedADTs ++ map adtName l

        getAbsentADTRefs :: [Sort] -> [Ref Name]
        getAbsentADTRefs [] = []
        getAbsentADTRefs (SortADT (Ref txt) : ss)
            | txt `notElem` allNamesTxt = Ref txt : getAbsentADTRefs ss
            | otherwise                 = getAbsentADTRefs ss
            where allNamesTxt = map Name.toText allNames
        getAbsentADTRefs (_ : ss) = getAbsentADTRefs ss

        nonConstructableTypes = snd $ verifyConstructableADTs (getSortADTs definedADTs, l)

getSortADTs :: [ADTDef v] -> [Sort]
getSortADTs = map (SortADT . Ref . Name.toText . adtName)

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
--   * A list of constructable 'Sort's
--
--   * A list of non-constructable 'ADTDef's
verifyConstructableADTs :: ([Sort], [ADTDef Name])
                        -> ([Sort], [ADTDef Name]) 
verifyConstructableADTs (constructableSorts, uADTDfs) =
    let (cs,ncs)  = partition
                        (any (allFieldsConstructable constructableSorts) . Map.elems . cDefsToMap . constructors)
                        uADTDfs
    in  if null cs
            then (constructableSorts,uADTDfs)
            else verifyConstructableADTs (getSortADTs cs ++ constructableSorts, ncs)

allFieldsConstructable :: [Sort] -> ConstructorDef Name -> Bool
allFieldsConstructable constructableSorts cDef = all (isSortConstructable constructableSorts) $ sortsOfFieldDefs $ fields cDef

isSortConstructable :: [Sort] -> Sort -> Bool
isSortConstructable constructableSorts fieldSort@(SortADT _) = fieldSort `elem` constructableSorts
isSortConstructable _                  _                     = True

-- | Creates a list of 'FieldDef.sort's of every 'FieldDef' in a 'FieldDefs'.
sortsOfFieldDefs :: FieldDefs Name -> [Sort]
sortsOfFieldDefs fDefs = map (read . T.unpack . Name.toText . sort) $ fDefsToList fDefs

-- | Type of errors that are raised when it's not possible to add 'ADTDef's to
--   'ADTDefs' structure via 'addADTDefs' function.
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
     deriving (Eq,Ord,Show,Read,Generic,NFData,Data)

instance Identifiable Sort where
    getId _ = Nothing

instance Resettable Sort where
    reset = id
