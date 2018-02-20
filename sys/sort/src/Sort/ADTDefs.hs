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
import           Data.List.Unique
import           Data.List        (intercalate,partition)
import qualified Data.Map.Strict  as Map
import           Data.Text        (Text)
import qualified Data.Text        as T
import           GHC.Generics     (Generic)

import           Id
import           Ref
import           Name
import           Sort.ConstructorDefs
import           Sort.ConvertsTo
import           Sort.FieldDefs

-- | Data structure for Abstract Data Type (ADT) definition.
data ADTDef v = ADTDef
    { adtName      :: Name              -- ^ Name of the ADT
    , constructors :: ConstructorDefs v -- ^ Constructor definitions of the ADT
    }
    deriving (Eq,Ord,Read,Show,Generic,NFData,Data)

instance HasName (ADTDef v) where
    getName = adtName

-- | Data structure for a collection of 'ADTDef's.
newtype ADTDefs = ADTDefs { -- | Transform 'ADTDefs' to a 'Data.Map.Map' from 'Ref' 'ADTDef' to 'ADTDef'.
                            adtDefsToMap :: Map.Map (Ref (ADTDef Sort)) (ADTDef Sort)
                              -- QUESTION: is this 'Ref' adding any type safety over just having just 'Text'?
                              --
                              -- I could always do something like:
                              --
                              -- > Map.lookup (Ref "ThisIsNotAnADT" :: Ref (ADTDef Sort)) adtDefs
                              --
                              -- And still write incorrect code ...
                              --
                              -- If we don't want this, we need make the 'Ref'
                              -- constructor private (WITHOUT exposing a smart
                              -- constructor either), which means that users of
                              -- this library ('sort') will only be able to
                              -- lookup references that exist (in which case I
                              -- wonder why do we have a map...).
                              --
                              -- I think we DO NOT want to make 'Ref' private:
                              -- consider what happens when we find a variable
                              -- declaration:
                              --
                              -- > p :: Person
                              --
                              -- If the keys of the map are of type 'Text' we
                              -- can readily perform the lookup. If they are
                              -- 'Ref' and 'Ref' constructor is private, then we
                              -- have no way of constructing a 'Ref' from the
                              -- string "Person".
                              --
                              -- My conclusion will be: let the fields be just
                              -- references.
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
-- TODO - QUESTION: why not SortPrim "Int" & SortADT "WhatEver"
data Sort = SortError -- TODO - QUESTION: why is Error a sort?
          | SortBool
          | SortInt
          | SortChar
          | SortString
          | SortRegex
          | SortADT (Ref (ADTDef Sort))
     deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

primitiveSortNames :: [Name]
primitiveSortNames = getName <$> ["Int", "Bool", "Char", "String", "Regex"] -- TODO: Do we need "Error" ?
    where
        getName :: String -> Name
        getName s = n
            where Right n = name $ T.pack s
        
sortFromName :: Name -> Sort
sortFromName nm = sortFromString $ Name.toText nm
    where
        sortFromString :: Text -> Sort
        sortFromString "Int" = SortInt
        sortFromString "Bool" = SortBool
        sortFromString "Char" = SortChar
        sortFromString "String" = SortString
        sortFromString "Regex" = SortRegex
        sortFromString "Error" = SortError
        sortFromString adtTxt = SortADT $ Ref adtTxt

instance Identifiable Sort where
    getId _ = Nothing

instance Resettable Sort where
    reset = id

instance ConvertsTo Name Sort where
    convertTo = sortFromName

instance ConvertsTo a a' => ConvertsTo (ADTDef a) (ADTDef a') where
    convertTo (ADTDef n cs) = ADTDef (convertTo n) (convertTo cs)
