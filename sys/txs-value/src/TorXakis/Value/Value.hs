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
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           TorXakis.Sort
import           TorXakis.Name

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
  
instance HasSort a Value where
    getSort _ Cbool{}         = SortBool
    getSort _ Cint{}          = SortInt
    getSort _ Cchar{}         = SortChar
    getSort _ Cstring{}       = SortString
    getSort _ Cregex{}        = SortRegex
    getSort _ (Ccstr a _ _)   = SortADT a
    getSort _ (Cany s)        = s
