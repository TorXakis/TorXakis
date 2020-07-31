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
module Constant
( 
-- * Data structure for constant definitions
  Constant (..)
)
where

import           Control.DeepSeq
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Data
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           CstrId
import           Id
import           SortId
import           SortOf

-- | Union of Boolean, Integer, String, and AlgebraicDataType constant values.
data Constant = -- | Constructor of Boolean constant.
                Cbool    { toBool :: Bool }
                -- | Constructor of Integer constant.
              | Cint     { toInteger :: Integer }
                -- | Constructor of String constant.
              | Cstring  { toText :: Text }
                -- | Constructor of Regular Expression constant.
              | Cregex   { -- | Regular Expression in XSD format
                           toXSDRegex :: Text } 
                                            -- PvdL: performance gain: translate only once,
                                            --       storing SMT string as well
                -- | Constructor of constructor constant (value of ADT).
              | Ccstr    { cstrId :: CstrId, args :: [Constant] }
                -- | Constructor of ANY constant.
              | Cany     { sort :: SortId }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Const is Serializable to/from JSON
instance ToJSON Constant
instance FromJSON Constant

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