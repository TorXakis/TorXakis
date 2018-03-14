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
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.Sort.ADTDefs
( -- * 'Sort's of Value Expressions
  Sort (..)

  -- * Abstract Data Types
  -- ** Data structure
, ADTDef (..)
, Unchecked
, U (U)
-- ** Collection
, ADTDefs (..)
, mergeADTDefs

-- ** Usage
, emptyADTDefs
, addADTDefs
, findConstructor
, getDefs  
-- , getConstructors

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
import           Control.Arrow       ((|||))
import           TorXakis.Sort.Ref
import           TorXakis.Sort.Name
import           TorXakis.Sort.ConstructorDefs
import           TorXakis.Sort.ConvertsTo

-- | Data structure for Abstract Data Type (ADT) definition.
data ADTDef sortRef = ADTDef
    { adtName      :: Name                    -- ^ Name of the ADT
    , constructors :: ConstructorDefs sortRef -- ^ Constructor definitions of the ADT
    }
    deriving (Eq, Read, Show, Generic, NFData, Data)

instance HasName (ADTDef sr) where
    getName = adtName

instance Referencable (ADTDef sr)

-- | Data structure for a collection of 'ADTDef's.
newtype ADTDefs = ADTDefs { -- | Transform 'ADTDefs' to a 'Data.Map.Map' from 'Ref' 'ADTDef' to 'ADTDef'.
                          adtDefsToMap :: Map.HashMap (Ref (ADTDef Sort)) (ADTDef Sort)
                          }
    deriving (Eq, Read, Show, Generic, NFData, Data)

-- | Smart constructor for 'ADTDefs'.
--
--   Creates an empty 'ADTDefs'.
emptyADTDefs :: ADTDefs
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
addADTDefs :: [ADTDef Unchecked] -- ^ Unchecked ADT definitions
            -> ADTDefs      -- ^ Available checked ADT definitions
            -> Either ADTError ADTDefs
addADTDefs as adfs
    | not $ null nuADTDefs   = Left $ NamesNotUnique nuADTDefs
    | not $ null unknownRefs = Left $ RefsNotFound unknownRefs
    | not $ null ncADTs      = Left $ NonConstructableTypes ncADTs
    | otherwise              = Right $ ADTDefs $ Map.union adtMap $ convertTo as
    where
        adtMap = adtDefsToMap adfs
        nuADTDefs = searchDuplicateNames2 as definedADTs
        unknownRefs = mapMaybe getUnknownADTRefs as
            where
                getUnknownADTRefs :: ADTDef Unchecked
                                  -> Maybe ([Unchecked], ADTDef Unchecked)
                getUnknownADTRefs aDef =
                    let xs = filter (not . isDefined) $ fieldSortNames aDef in
                        if null xs
                        then Nothing
                        else Just (xs, aDef)

                fieldSortNames :: ADTDef Unchecked -> [Unchecked]
                fieldSortNames adt = getAllFieldSortNames $ constructors adt

                isDefined :: Unchecked -> Bool
                isDefined (U (Left s))  = isPrim s
                isDefined (U (Right n)) = Map.member (RefByName n) adtMap
                                      || n `elem` newADTNames
                    where newADTNames = map adtName as
        definedADTs = Map.elems adtMap
        -- TODO: discuss with Pierre and Kerem, having to construct a sort from
        -- name here seems plainly wrong.
        ncADTs = verifyConstructibleADTs (map (U . Left . sortFromName . adtName) definedADTs) as
            where
                -- | Verifies if given list of 'ADTDef's are constructable.
                --
                --   Input:
                --
                --   * A list of known constructable Sorts or names of the ADT definitions.
                --
                --   * A list of 'ADTDef's to be verified
                --
                --   Output: A tuple consisting of:
                --
                --   * A list of non-constructable 'ADTDef's
                --
                verifyConstructibleADTs ::[Unchecked]
                                        -> [ADTDef Unchecked]
                                        -> [ADTDef Unchecked]
                verifyConstructibleADTs constructableSortNames uADTDfs =
                    let (cs, ncs)  = partition
                                    (any (allFieldsConstructable constructableSortNames) . getConstructors)
                                    uADTDfs
                    in if null cs
                    then uADTDfs
                    else verifyConstructibleADTs
                         (map (U . Right . adtName) cs ++ constructableSortNames)
                         ncs
                allFieldsConstructable :: [Unchecked] -> ConstructorDef Unchecked -> Bool
                allFieldsConstructable constructableSortNames cDef =
                    all (isSortConstructable constructableSortNames)
                        $ getFieldSorts cDef
                isSortConstructable :: [Unchecked] -> Unchecked -> Bool
                isSortConstructable ns esn =
                    isPrimSort esn || esn `elem` ns
                    where
                      isPrimSort (U (Left s))  = isPrim s
                      isPrimSort (U (Right _)) = False

getDefs :: ADTDefs -> [ADTDef Sort]
getDefs = Map.elems . adtDefsToMap

-- | Merges two 'ADTDefs' structures.
--
--   Preconditions:
--
--   * If names of two 'ADTDef's match, their constructors' names and fields
--     should also match.
--
--   Given two 'ADTDefs'
--
--   * either an error message indicating some violations of preconditions
--
--   * or an 'ADTDefs' structure
--
--   is returned.
--
-- Since both parameters are ADTDefs, we don't have to do same verifications above.
--
-- TODO: discuss why do we need this? Doesn't the @addADTDefs@ smart
-- constructor already have a parameter for merging an ADT into an existing
-- strcture?
mergeADTDefs :: ADTDefs -> ADTDefs -> Either ADTError ADTDefs
mergeADTDefs adts1@(ADTDefs dsMap1) adts2@(ADTDefs dsMap2)
    | dsMap1 == Map.empty = Right adts2
    | dsMap2 == Map.empty = Right adts1
    | otherwise           = undefined
    -- TODO: Check for conflicting ADTDef's via intersection: If names match,
    --       EVERYTHING ELSE should also match so that one of them can be
    --       dropped while combining.
    -- TODO: You can use Map.Union to combine.

-- | Returns a 'ConstructorDef' and its reference from a given 'ADTDef' based on
--   its name.
findConstructor :: Name -> ADTDef v -> Maybe (Ref (ConstructorDef v), ConstructorDef v)
findConstructor nm ad = case Map.lookup r $ (cDefsToMap . constructors) ad of
                            Nothing -> Nothing
                            Just cd -> Just (r,cd)
                    where r = RefByName nm

-- | Returns the list of 'ConstructorDef's of a given 'ADTDef'.
getConstructors :: ADTDef v -> [ConstructorDef v]
getConstructors = Map.elems . cDefsToMap . constructors

-- | Checks if a given 'Sort' 'Name' is defined as a primitive sort or an ADT.
-- isSortDefined :: ADTDefs -> Name -> Bool
-- isSortDefined ads n = Map.member (RefByName n) (adtDefsToMap ads)
--                     || n `elem` primitiveSortNames


-- | Type of errors that are raised when it's not possible to add 'ADTDef's to
--   'ADTDefs' structure via 'addADTDefs' function.
data ADTError = RefsNotFound          [([Unchecked], ADTDef Unchecked)]
              | NamesNotUnique        [ADTDef Unchecked]
              | NonConstructableTypes [ADTDef Unchecked]
        deriving (Eq)

instance Show ADTError where
    show (RefsNotFound                       []) = ""
    show (RefsNotFound ( (uNms,reqADTDf) : ts) ) = "Sorts(s) " ++ showReqSorts
                                                ++ " required by ADT '" ++ (show . adtName) reqADTDf
                                                ++ "' are not defined.\n"
                                                ++ show (RefsNotFound ts)
        where showReqSorts =
                  T.unpack (T.intercalate "," $ map (T.pack . show) uNms)
    show (NamesNotUnique                  aDefs) = "Names of following ADT definitions are not unique: "
                                                ++ show aDefs
    show (NonConstructableTypes           aDefs) = "ADTs are not constructable: "
                                                ++ intercalate ", " (map (show . adtName) aDefs)

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
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)
-- If we want to make Sort package more flexible, we can use SortPrim "Int" & SortADT "WhatEver".

-- | Is the sort primitive?
isPrim :: Sort -> Bool
isPrim (SortADT _) = False
isPrim _           = True

-- | Creates corresponding 'Sort' for a given 'Name'.
sortFromName :: Name -> Sort
sortFromName nm = sortFromText $ toText nm
    where
        sortFromText :: Text -> Sort
        sortFromText adtTxt = SortADT $ RefByName adtNm
            where Right adtNm = mkName adtTxt

instance ConvertsTo Name Sort where
    convertTo = sortFromName

instance ConvertsTo Sort Sort where
    convertTo = id

instance ConvertsTo a a' => ConvertsTo (ADTDef a) (ADTDef a') where
    convertTo (ADTDef n cs) = ADTDef n (convertTo cs)

-- | Type of an unchecked field
type Unchecked = U Sort Name

-- | Type wrapper to avoid defining an orphan instance of 'ConvertsTo'.
-- Defining this instance in the 'ConvertsTo' module won't work since this
-- module depends on 'ConvertsTo' and importing 'Sort' will cause a circular
-- dependency.
newtype U a b = U { toEither :: Either a b } deriving (Show, Eq)

instance ConvertsTo (U Sort Name) Sort where
    convertTo = (convertTo ||| convertTo) . toEither

instance ConvertsTo (U Sort Name) (U Sort Name) where
    convertTo = id
