{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Value
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structure for value definitions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.Value.Value
( 
-- * Data structure for value definitions
  ValueView (..)
, Value
, view
-- ** Constructors for values.
, mkBool
, mkInt
, mkString
, mkADT
, mkANY
)
where

import           Control.DeepSeq (NFData)
import           Data.Data       (Data)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortContext

-- | ValueView: the public view of a value 'Value'.
-- | Union of Boolean, Integer, Char, String, and AlgebraicDataType value values.
data ValueView =  -- | Boolean value.
                  Cbool Bool
                  -- | Int value.
                | Cint Integer
                  -- | String value.
                | Cstring Text
                  -- | ADT value.
                | Cadt (RefByName ADTDef) (RefByName ConstructorDef) [Value]
                  -- | ANY value - temporary hack : don't use.
                  -- ANY will be replaced by Maybe Sort when polymorphic types are supported.
                  -- See https://en.wikipedia.org/wiki/Parametric_polymorphism for more info.
                | Cany Sort
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Value
--
-- 1. User can't directly construct Value (such that invariants will always hold)
--
-- 2. User can still pattern match on Value using 'ValueView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype Value = Value { -- | View on value.
                        view :: ValueView
                      }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Constructor for Bool value
mkBool :: Bool -> Value
mkBool = Value . Cbool

-- | Constructor for Int value
mkInt :: Integer -> Value
mkInt = Value . Cint

-- | Constructor for String value
mkString :: Text -> Value
mkString = Value . Cstring

-- | Constructor for ADT value
mkADT :: SortContext c => c -> RefByName ADTDef -> RefByName ConstructorDef -> [Value] -> Either Error Value
mkADT ctx aRef cRef vs =
        case lookupADT (toName aRef) ctx of
             Nothing   -> Left $ Error ("ADTDefinition " ++ show aRef ++ " not defined in context")
             Just aDef -> case lookupConstructor (toName cRef) aDef of
                             Nothing   -> Left $ Error ("Constructor " ++ show cRef ++ " not defined for ADTDefinition " ++ show aRef)
                             Just cDef -> let expectedSorts = map (getSort ctx) (elemsField cDef) 
                                              actualSorts = map (getSort ctx) vs
                                            in
                                                if expectedSorts == actualSorts
                                                then Right $ Value (Cadt aRef cRef vs)
                                                else Left $ Error ("Mismatch in sorts:\nexpected = "++ show expectedSorts ++ "\nactual = " ++ show actualSorts)

-- | Constructor for ANY value
-- temporary hack : don't use.
-- ANY will be replaced by Maybe Sort when polymorphic types are supported.
-- See https://en.wikipedia.org/wiki/Parametric_polymorphism for more info.
mkANY :: Sort -> Value
mkANY = Value . Cany

instance HasSort c Value where
    getSort ctx = getSort ctx . view

instance HasSort c ValueView where
    getSort _ Cbool{}      = SortBool
    getSort _ Cint{}       = SortInt
    getSort _ Cstring{}    = SortString
    getSort _ (Cadt a _ _) = SortADT a
    getSort _ (Cany s)     = s

instance SortContext c => UsedSorts c Value where
    usedSorts ctx = usedSorts ctx . view

instance SortContext c => UsedSorts c ValueView where
    usedSorts _   Cbool{}       = Set.singleton SortBool
    usedSorts _   Cint{}        = Set.singleton SortInt
    usedSorts _   Cstring{}     = Set.singleton SortString
    usedSorts ctx (Cadt a _ vs) = Set.insert (SortADT a) $ Set.unions (map (usedSorts ctx) vs)
    usedSorts _   (Cany s)      = Set.singleton s