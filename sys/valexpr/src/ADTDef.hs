{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ADTDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
--                kerem.ispirli@tno.nl
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structures and constructors for Abstract Data Types.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
module ADTDef
( ADTDef (..)
, ADTDefs
, ADTDef.toMap
, ADTDef.empty
, addADTDefs
, getADTDef
)
where

import           Control.DeepSeq
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text
import           GHC.Generics    (Generic)

import           ConstructorDef
import           Ref

-- | Data structure for Abstract Data Type (ADT) definition.
data ADTDef = ADTDef
    { name :: Text                                   -- ^ Name of the ADT
    , constructors :: ConstructorDef.ConstructorDefs -- ^ Constructor definitions of the ADT
    }
    deriving (Eq,Ord,Read,Show,Generic,NFData)

-- | Data structure for a collection of Abstract Data Type (ADT) definitions.
newtype ADTDefs = ADTDefs { -- | Transform 'ADTDefs' to a map from 'TRef' 'ADTDef' to 'ADTDef'.
                            toMap :: Map.Map (TRef ADTDef) ADTDef
                          }
    deriving (Eq,Ord,Read,Show,Generic,NFData)

-- | Smart constructor for 'ADTDefs'.
--
--   Creates an empty Abstract Data Type (ADT) collection.
empty ::  ADTDefs
empty = ADTDefs Map.empty

-- | Smart constructor for 'ADTDefs'.
--
--   todo: Given a list of tuples of 'TRef' 'ADTDef' and 'ADTDef', and an 'ADTDefs'
--
--   * either an error message about one or more of the following violations:
--
--       * References and/or names of the definitions are not unique
--
--       * References are not defined
--
--       * Data types cannot be constructed
--
--   * or a structure containing all data types
--
--   is returned.
addADTDefs :: [(TRef ADTDef, ADTDef)]
           -> ADTDefs
           -> Either String ADTDefs
addADTDefs l adfs =
    let adtMap = ADTDef.toMap adfs
        nonUniqueRefs  = repeated $ map fst l
        nonUniqueNames = repeated $ map ( name . snd ) l
    in  
        Right $ ADTDefs $ Map.union adtMap $ Map.fromList l

-- | TODO: Document
getADTDef :: TRef ADTDef -> ADTDefs -> ADTDef
getADTDef r = fromMaybe (error "adtRef not found")
              . Map.lookup r . ADTDef.toMap
