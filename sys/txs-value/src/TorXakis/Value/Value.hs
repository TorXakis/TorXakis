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
  Value (..)
)
where

import           Control.DeepSeq (NFData)
import           Data.Data       (Data)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortContext

-- | Union of Boolean, Integer, Char, String, and AlgebraicDataType value values.
data Value = -- | Constructor of Boolean value.
             Cbool Bool
             -- | Constructor of Int value.
           | Cint Integer
--             -- | Constructor of Char value.
--           | Cchar Char
             -- | Constructor of String value.
           | Cstring Text
             -- | Constructor of constructor value (value of ADT).
           | Ccstr (RefByName ADTDef) (RefByName ConstructorDef) [Value]
             -- | Constructor of ANY value - temporary hack : don't use.
             -- ANY will be replaced by Maybe Sort when polymorphic types are supported.
             -- See https://en.wikipedia.org/wiki/Parametric_polymorphism for more info.
           | Cany Sort
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)
  
instance HasSort c Value where
    getSort _ Cbool{}         = SortBool
    getSort _ Cint{}          = SortInt
--  getSort _ Cchar{}         = SortChar
    getSort _ Cstring{}       = SortString
    getSort _ (Ccstr a _ _)   = SortADT a
    getSort _ (Cany s)        = s

instance SortContext c => UsedSorts c Value where
    usedSorts _   Cbool{}         = Set.singleton SortBool
    usedSorts _   Cint{}          = Set.singleton SortInt
--  usedSorts _   Cchar{}         = Set.singleton SortChar
    usedSorts _   Cstring{}       = Set.singleton SortString
    usedSorts ctx (Ccstr a _ vs)  = Set.insert (SortADT a) $ Set.unions (map (usedSorts ctx) vs)
    usedSorts _   (Cany s)        = Set.singleton s