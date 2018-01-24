{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Identifier
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
--                kerem.ispirli@tno.nl
-- Stability   :  experimental
-- Portability :  portable
--
-- functionality to replace identifiers (names and signatures) by references
-----------------------------------------------------------------------------
module Identifier
( Identifier (..)
, IdentifierToReference
, Identifier.empty
, addIdentifier
, getReference
, Ref
, toInt
)
where

import Data.Text (Text)
import Data.Map  as Map
import Data.Maybe

import Ref

-- | Data structure for Identifier
newtype Identifier = Name Text
    deriving (Eq, Ord)

-- | Data structure for replacing identifiers by references.
data IdentifierToReference = IdentifierToReference { toMap :: Map.Map Identifier Int
                                                   , nextRef :: Int
                                                   }

empty :: IdentifierToReference
empty = IdentifierToReference Map.empty 0

-- | add 'Identifier' to 'IdentifierToReference'
addIdentifier :: Identifier -> IdentifierToReference -> IdentifierToReference
addIdentifier i IdentifierToReference{toMap = m, nextRef = nr} = IdentifierToReference (Map.insert i nr m) $ nr+1

-- | get reference of 'Identifier' earlier added to 'IdentifierToReference'
getReference :: t -> Identifier -> IdentifierToReference -> Ref t
getReference _ i IdentifierToReference{toMap = m} =
    Ref $ fromMaybe (error "Identifier not found") $ Map.lookup i m
