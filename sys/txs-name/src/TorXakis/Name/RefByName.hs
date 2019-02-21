{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RefByName
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
module TorXakis.Name.RefByName
( -- * Reference By Name
  RefByName (..)
) where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))

import           TorXakis.Name.Name

-- | A generalized, type-safe reference.
newtype RefByName t = RefByName { -- | This reference keeps a 'Name' that represents the entity.
                                  toName :: Name
                                }
    deriving (Eq, Ord, Show, Read, NFData, Data)

instance Hashable (RefByName t) where
    hashWithSalt s = hashWithSalt s . toName
