{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Constant
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structure for constant definitions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
module Constant
( 
-- * Data structure for constant definitions
  Constant (..)
-- * Getter and Setter of Lens
, toBool
, Constant.toInteger
, toText
, toXSDRegex
, cstrId
, args
, sort
)
where

import           Control.DeepSeq
import           Control.Lens
import           Data.Data
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           CstrId
import           Id
import           SortId
import           SortOf

-- | Union of Boolean, Integer, String, and AlgebraicDataType constant values.
data Constant = -- | Constructor of Boolean constant.
                Cbool    { _toBool :: Bool }
                -- | Constructor of Integer constant.
              | Cint     { _toInteger :: Integer }
                -- | Constructor of String constant.
              | Cstring  { _toText :: Text }
                -- | Constructor of Regular Expression constant.
              | Cregex   { -- | Regular Expression in XSD format
                           _toXSDRegex :: Text } 
                                            -- PvdL: performance gain: translate only once,
                                            --       storing SMT string as well
                -- | Constructor of constructor constant (value of ADT).
              | Ccstr    { _cstrId :: CstrId, _args :: [Constant] }
                -- | Constructor of ANY constant.
              | Cany     { _sort :: SortId }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)
makeLenses ''Constant

-- | Const is Resettable
instance Resettable Constant

-- | Const has a Sort.
instance SortOf Constant where
  sortOf (Cbool _b)                         = sortIdBool
  sortOf (Cint _i)                          = sortIdInt
  sortOf (Cstring _s)                       = sortIdString
  sortOf (Cregex _r)                        = sortIdRegex
  sortOf (Ccstr (CstrId _nm _uid _ca cs) _) = cs
  sortOf (Cany s)                           = s