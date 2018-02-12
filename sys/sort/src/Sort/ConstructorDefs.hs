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
module Sort.ConstructorDefs
( -- * Constructors
  -- ** Data structure
  ConstructorDef (..)

-- ** Collection
, ConstructorDefs (..)

-- ** Usage
, constructorDefs

-- * ADT Constructor Errors
, ADTConstructorError (..)
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.List.Unique
import           Data.List        (intercalate)
import qualified Data.Map.Strict  as Map
import           GHC.Generics     (Generic)

import           Ref
import           Name
import           Sort.FieldDefs

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
constructorDefs :: [ConstructorDef Name]
                -> Either ADTConstructorError (ConstructorDefs Name)
constructorDefs [] = Left EmptyConstructorDefs
constructorDefs l
    | not $ null nuCstrNames    = let nonUniqDefs = filter ((`elem` nuCstrNames) . constructorName) l
                                  in  Left $ ConstructorNamesNotUnique nonUniqDefs
    | not $ null nuFieldNames   = Left $ SameFieldMultipleCstr nuFieldNames
    | otherwise = Right $ ConstructorDefs $ Map.fromList $ map (\cd -> (Ref $ Name.toText $ constructorName cd, cd)) l
    where
        nuCstrNames    = repeated $ map constructorName l
        nuFieldNames   = repeated $ map fieldName $ concatMap (fDefsToList . fields) l

data ADTConstructorError = ConstructorNamesNotUnique [ConstructorDef Name]
                         | EmptyConstructorDefs
                         | SameFieldMultipleCstr     [Name]
    deriving (Eq)

instance Show ADTConstructorError where
    show (ConstructorNamesNotUnique cDefs) = "Names of following constructor definitions are not unique: " ++ show cDefs
    show  EmptyConstructorDefs             = "No constructor definitions provided."
    show (SameFieldMultipleCstr     names) = "Field names in multiple constructors: "
                                                ++ intercalate ", " (map show names)
