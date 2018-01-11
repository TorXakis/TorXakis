{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ConstructorDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
--                kerem.ispirli@tno.nl
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structure for constructors
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module ConstructorDef
( ConstructorDef (..)
, ConstructorDefs
, constructorDefs
, toMap
, getConstructorDef
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.List.Unique
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as Text
import           GHC.Generics (Generic)

import qualified FieldDef
import Ref

-- | Data structure for constructor definition.
data ConstructorDef = ConstructorDef { name :: Text.Text            -- ^ Name of the constructor
                                     , fields :: FieldDef.FieldDefs -- ^ Field definitions of the constructor
                                     }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | Data structure for a collection of constructor definitions.
newtype ConstructorDefs = ConstructorDefs { -- | Transform 'ConstructorDefs' to a map from 'TRef' 'ConstructorDef' to 'ConstructorDef'.
                                            toMap :: Map.Map (TRef ConstructorDef) ConstructorDef
                                          }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | Smart constructor for 'ConstructorDefs'.
--
--   Given a list of tuples of 'TRef' 'ConstructorDef' and 'ConstructorDef',
--
--   * either an error message, telling which references and/or names of the
--     definitions are not unique, (todo: Field names should also be unique across all Constructors)
--
--   * or a structure containing the constructor definitions
--
--   is returned.
constructorDefs :: [(TRef ConstructorDef, ConstructorDef)]
                -> Either String ConstructorDefs
constructorDefs l = let nonUniqueRefs  = repeated $ map fst l
                        nonUniqueNames = repeated $ map ( name . snd ) l
                    in if null nonUniqueRefs && null nonUniqueNames
                           then Right $ ConstructorDefs $ Map.fromList l
                           else let refErr = if null nonUniqueRefs
                                    then let nonUniqTuples = filter ((`elem` nonUniqueRefs) . fst) l
                                        in  "Refs are not unique: " ++ show nonUniqTuples
                                    else ""
                                    nameErr= if null nonUniqueNames
                                        then let nonUniqTuples = filter ((`elem` nonUniqueNames) . name . snd) l
                                            in  "Names are not unique: " ++ show nonUniqTuples
                                        else ""
                        in  Left $ refErr ++ "\n" ++ nameErr

-- | TODO: Document
getConstructorDef :: TRef ConstructorDef -> ConstructorDefs -> ConstructorDef
getConstructorDef r = fromMaybe (error "ConstructorDef not found")
                      . Map.lookup r . ConstructorDef.toMap
