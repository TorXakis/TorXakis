{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Const
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
module Const
( 
-- * Data structure for constant definitions
  Const.Const (..)
-- * Getter and Setter of Lens
, toBool
, Const.toInteger
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
data Const = -- | Constructor of Boolean constant.
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
              | Ccstr    { _cstrId :: CstrId, _args :: [Const.Const] }
                -- | Constructor of ANY constant.
              | Cany     { _sort :: SortId }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)
makeLenses ''Const.Const

-- | Const is Resettable
instance Resettable Const.Const

-- | Const has a Sort.
instance SortOf Const.Const where
  sortOf (Cbool _b)                         = sortIdBool
  sortOf (Cint _i)                          = sortIdInt
  sortOf (Cstring _s)                       = sortIdString
  sortOf (Cregex _r)                        = sortIdRegex
  sortOf (Ccstr (CstrId _nm _uid _ca cs) _) = cs
  sortOf (Cany s)                           = s