{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FieldDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation)
--                kerem.ispirli@tno.nl
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structure for fields
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module FieldDef
( FieldDef (..)
, FieldDefs
, fieldDefs
, toList
, nrOfFieldDefs
, sortsOfFieldDefs
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.List.Unique
import qualified Data.Text as Text
import           GHC.Generics (Generic)

import Ref
import SortDef

-- | Data structure for a field definition.
data FieldDef = FieldDef { name       :: Text.Text    -- ^ Name of the field
                         , refSortDef :: TRef SortDef -- ^ Sort reference of the field
                         }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | Data structure for a collection of field definitions.
data FieldDefs = FieldDefs  { -- | Transform 'FieldDefs' to a list of tuples of 'TRef' 'FieldDef' and 'FieldDef'.
                              toList :: [(TRef FieldDef, FieldDef)]
                              -- | Number of field definitions
                            , nrOfFieldDefs :: Int
                            }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Smart constructor for 'FieldDefs'.
--
--   Given a list of tuples of 'TRef' 'FieldDef' and 'FieldDef',
--
--   * either an error message, telling which references and/or names of the
--     definitions are not unique,
--
--   * or a structure containing the field definitions
--
--   is returned.
--
--   Note, the position in the list is relevant for representations with implicit positions.
fieldDefs :: [(TRef FieldDef, FieldDef)] -> Either String FieldDefs
fieldDefs l = let nonUniqueRefs  = repeated $ map fst l
                  nonUniqueNames = repeated $ map ( name . snd ) l
              in if null nonUniqueRefs && null nonUniqueNames
                    then Right $ FieldDefs l $ length l
                    else let refErr =
                                if null nonUniqueRefs
                                    then let nonUniqTuples = filter ((`elem` nonUniqueRefs) . fst) l
                                         in  "Refs are not unique: " ++ show nonUniqTuples
                                    else ""
                             nameErr=
                                if null nonUniqueNames
                                    then let nonUniqTuples = filter ((`elem` nonUniqueNames) . name . snd) l
                                         in  "Names are not unique: " ++ show nonUniqTuples
                                    else ""
                  in  Left $ refErr ++ "\n" ++ nameErr

sortsOfFieldDefs :: FieldDefs -> [TRef SortDef]
sortsOfFieldDefs = map (refSortDef . snd) . toList