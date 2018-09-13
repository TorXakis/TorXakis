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
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.Value.Value
( 
-- * Data structure for value definitions
  Value (..)
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           TorXakis.Name
import           TorXakis.Sort

-- | Union of Boolean, Integer, Char, String, and AlgebraicDataType value values.
data Value = -- | Constructor of Boolean value.
             Cbool Bool
             -- | Constructor of Int value.
           | Cint Integer
             -- | Constructor of Char value.
           | Cchar Char
             -- | Constructor of String value.
           | Cstring Text
             -- | Constructor of Regular Expression value (in XSD format).
           | Cregex Text                 -- PvdL: performance gain: translate only once,
                                         --       storing SMT string as well
             -- | Constructor of constructor value (value of ADT).
           | Ccstr (RefByName ADTDef) (RefByName ConstructorDef) [Value]
             -- | Constructor of ANY value - temporary hack : don't use.
           | Cany Sort -- TODO: replace by generic - Maybe
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)
  
instance HasSort Value where
    getSort Cbool{}         = SortBool
    getSort Cint{}          = SortInt
    getSort Cchar{}         = SortChar
    getSort Cstring{}       = SortString
    getSort Cregex{}        = SortRegex
    getSort (Ccstr a _ _)   = SortADT a
    getSort (Cany s)        = s
