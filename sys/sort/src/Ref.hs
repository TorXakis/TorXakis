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
, TRef(..)
) where

import           Control.DeepSeq
import           Data.Data
import           Id

-- | A reference that enables fast comparison.
newtype Ref = Ref { -- | A reference keeps an 'Int' for fast comparison.
                    toInt :: Int
                  }
    deriving (Eq, Ord, Read, Show, NFData, Data)

instance Identifiable Ref where
    getId = Just . Id . toInt

instance Resettable Ref where
    reset _ = Ref 0

-- | Typed 'Ref', for type-safety.
newtype TRef t = TRef Ref
    deriving (Eq, Ord, Read, Show, NFData, Data)

instance Identifiable (TRef t) where
    getId (TRef r)= Just $ Id $ toInt r

instance Resettable (TRef t) where
    reset _ = TRef $ Ref 0
    