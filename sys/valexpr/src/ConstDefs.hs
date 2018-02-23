{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ConstDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structure for constant definitions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module ConstDefs
where

import           Control.DeepSeq
import           Data.Data
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           Sort
import           Id
import           SortOf

-- | Union of Boolean, Integer, String, and AlgebraicDataType constant values.
data Const = Cbool    { cBool :: Bool }
           | Cint     { cInt :: Integer }
           | Cstring  { cString :: Text }
           | Cregex   { cRegex :: Text } -- ^ XSD input
                                         -- PvdL: performance gain: translate only once,
                                         --       storing SMT string as well
           | Cstr     { adtRef  :: Ref (ADTDef Sort)
                      , cstrRef :: Ref (ConstructorDef Sort)
                      , args    :: [Const]
                      }
           | Cerror   { msg  :: String }
           | Cany     { sort :: Sort }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Const is Resettable
instance Resettable Const

-- | Const has a Sort.
instance SortOf Const where
  sortOf (Cbool _b)                        = SortBool
  sortOf (Cint _i)                         = SortInt
  sortOf (Cstring _s)                      = SortString
  sortOf (Cregex _r)                       = SortRegex
  sortOf (Cstr adtRf _cstrRef _args)       = SortADT adtRf
  sortOf (Cany s)                          = s
  sortOf (Cerror _)                        = SortError