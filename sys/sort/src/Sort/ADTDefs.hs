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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sort.ADTDefs
( -- * 'Sort's of Value Expressions
  Sort (..)
, sortFromName

  -- * Abstract Data Types
  -- ** Data structure
, ADTDef (..)

-- ** Collection
, ADTDefs (..)

-- ** Usage
, emptyADTDefs
, addADTDefs
, getConstructors

-- * ADT Errors
, ADTError (..)
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.Maybe
import           Data.List           (intercalate,partition)
import qualified Data.HashMap.Strict as Map
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           Id
import           Ref
import           Name
import           Sort.ConstructorDefs
import           Sort.ConvertsTo
import           Sort.FieldDefs

-- | Data structure for Abstract Data Type (ADT) definition.
data ADTDef sortRef = ADTDef
    { adtName      :: Name                    -- ^ Name of the ADT
    , constructors :: ConstructorDefs sortRef -- ^ Constructor definitions of the ADT
    }
    deriving (Eq, Read, Show, Generic, NFData, Data)

instance HasName (ADTDef sr) where
    getName = adtName

instance Referencable (ADTDef sr) where
    mkRef = RefByName . adtName

-- | Data structure for a collection of 'ADTDef's.
newtype ADTDefs = ADTDefs { -- | Transform 'ADTDefs' to a 'Data.Map.Map' from 'Ref' 'ADTDef' to 'ADTDef'.
                          adtDefsToMap :: Map.HashMap (Ref (ADTDef Sort)) (ADTDef Sort)
                          }
    deriving (Eq, Read, Show, Generic, NFData, Data)

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
--
-- TorXakis does a simple type inference on the type of constructors, so there
-- is no need to check for duplicated constructor names for different ADTs.
addADTDefs :: [ADTDef Name] -- ^ Unchecked ADT definitions
            -> ADTDefs      -- ^ Available checked ADT definitions
            -> Either ADTError ADTDefs
addADTDefs as adfs
    | not $ null nuADTDefs   = Left $ NamesNotUnique nuADTDefs
    | not $ null unknownRefs = Left $ RefsNotFound unknownRefs
    | not $ null ncADTs      = Left $ NonConstructableTypes ncADTs
    | otherwise              = Right $ ADTDefs $ Map.union adtMap $ convertTo as
    where
        adtMap = adtDefsToMap adfs
        definedADTs = Map.elems adtMap
        nuADTDefs = searchDuplicateNames2 as definedADTs
        unknownRefs = mapMaybe getUnknownADTRefs as
            where
                getUnknownADTRefs :: ADTDef Name -> Maybe ([Name], ADTDef Name)
                getUnknownADTRefs aDef =
                    let xs = filter (not . isDefined) $ fieldSortNames aDef in
                        if null xs
                        then Nothing
                        else Just (xs, aDef)

                fieldSortNames :: ADTDef Name -> [Name]
                fieldSortNames adt = map sort $ concatMap (fDefsToList . fields)
                                    $ getConstructors adt

                isDefined :: Name -> Bool
                isDefined n = n `elem` (primitiveSortNames ++ allADTNames)
                    where allADTNames = map adtName definedADTs ++ map adtName as

        ncADTs = verifyConstructibleADTs (map adtName definedADTs) as
            where
                -- | Verifies if given list of 'ADTDef's are constructable.
                --
                --   Input:
                --
                --   * A list of known constructable 'ADTDef's
                --
                --   * A list of 'ADTDef's to be verified
                --
                --   Output: A tuple consisting of:
                --
                --   * A list of non-constructable 'ADTDef's
                --
                verifyConstructibleADTs ::[Name] -> [ADTDef Name] -> [ADTDef Name]
                verifyConstructibleADTs constructableSortNames uADTDfs =
                    let (cs, ncs)  = partition
                                    (any (allFieldsConstructable constructableSortNames) . getConstructors)
                                    uADTDfs
                    in if null cs
                    then uADTDfs
                    else verifyConstructibleADTs (map adtName cs ++ constructableSortNames) ncs
                allFieldsConstructable :: [Name] -> ConstructorDef Name -> Bool
                allFieldsConstructable constructableSortNames cDef =
                    all (isSortConstructable constructableSortNames)
                        $ (map sort . fDefsToList . fields) cDef
                isSortConstructable :: [Name] -> Name -> Bool
                isSortConstructable cSortNames sName =
                    sName `elem` (primitiveSortNames ++ cSortNames)

getConstructors :: ADTDef v -> [ConstructorDef v]
getConstructors = Map.elems . cDefsToMap . constructors

-- | Type of errors that are raised when it's not possible to add 'ADTDef's to
--   'ADTDefs' structure via 'addADTDefs' function.
data ADTError = RefsNotFound          [([Name], ADTDef Name)]
              | NamesNotUnique        [ADTDef Name]
              | NonConstructableTypes [ADTDef Name]
        deriving (Eq)

instance Show ADTError where
    show (RefsNotFound                       []) = ""
    show (RefsNotFound ( (uNms,reqADTDf) : ts) ) = "ADT(s) " ++ T.unpack (T.intercalate "," $ map Name.toText uNms)
                                                ++ " required by ADT '" ++ (show . adtName) reqADTDf
                                                ++ "' are not defined.\n"
                                                ++ show (RefsNotFound ts)
    show (NamesNotUnique                  aDefs) = "Names of following ADT definitions are not unique: "
                                                ++ show aDefs
    show (NonConstructableTypes           aDefs) = "ADTs are not constructable: "
                                                ++ intercalate ", " (map (show . adtName) aDefs)
    
-----------------------------------------------------------------------------
-- Sort
-----------------------------------------------------------------------------
-- | The data type that represents 'Sort's for 'ValExpr.ValExpr's.
data Sort = SortError -- TODO: Make an issue about removing Error sort?
          | SortBool
          | SortInt
          | SortChar
          | SortString
          | SortRegex
          | SortADT (Ref (ADTDef Sort))
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)
-- If we want to make Sort package more flexible, we can use SortPrim "Int" & SortADT "WhatEver".

primitiveSortNames :: [Name]
primitiveSortNames = mkName <$> ["Int", "Bool", "Char", "String", "Regex"]
    where
        mkName :: String -> Name
        mkName s = n
            where Right n = name $ T.pack s
        
sortFromName :: Name -> Sort
sortFromName nm = sortFromText $ Name.toText nm
    where
        sortFromText :: Text -> Sort
        sortFromText "Int" = SortInt
        sortFromText "Bool" = SortBool
        sortFromText "Char" = SortChar
        sortFromText "String" = SortString
        sortFromText "Regex" = SortRegex
        sortFromText adtTxt = SortADT $ RefByName adtNm
            where Right adtNm = name adtTxt

instance Identifiable Sort where
    getId _ = Nothing

instance Resettable Sort where
    reset = id

instance ConvertsTo Name Sort where
    convertTo = sortFromName

instance ConvertsTo a a' => ConvertsTo (ADTDef a) (ADTDef a') where
    convertTo (ADTDef n cs) = ADTDef (convertTo n) (convertTo cs)
