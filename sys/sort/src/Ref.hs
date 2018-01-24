{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Ref
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

module Ref
( Ref (..)
) where

import           Control.DeepSeq
import           Data.Data
import           Id

-- | A type-safe reference that enables fast comparison.
newtype Ref t = Ref { -- | A reference keeps an 'Int' for fast comparison.
                      toInt :: Int
                    }
    deriving (Eq, Ord, Read, Show, NFData, Data)

instance Identifiable (Ref t) where
    getId = Just . Id . toInt

instance Resettable (Ref t) where
    reset _ = Ref 0
