{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RefByIndex
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Kerem Ispirli <kerem.ispirli@tno.nl>
--                Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides a generalized, type-safe reference.
-----------------------------------------------------------------------------
module TorXakis.RefByIndex
( -- * Reference By Index
  RefByIndex (..)
) where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))

-- | A generalized, type-safe reference.
newtype RefByIndex t = RefByIndex { -- | This reference keeps an index to point to the entity.
                                  toIndex :: Int
                                }
    deriving (Eq, Ord, Show, Read, NFData, Data)

instance Hashable (RefByIndex t) where
    hashWithSalt s = hashWithSalt s . toIndex
