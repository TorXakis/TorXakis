{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
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
, Referencable (..)
) where

import           Control.DeepSeq
import           Data.Data
import           Data.Hashable

import           Id
import           Name

-- | A type-safe reference
newtype Ref t = RefByName { -- | This reference keeps a text that represents the entity.
                            toName :: Name
                          }
                -- We will introduce RefBySignature for functions; that's why
                -- the constructor name is different than type name.
    deriving (Eq, Ord, Show, Read, NFData, Data)

instance Identifiable (Ref t) where
    getId _ = Nothing

instance Resettable (Ref t) where
    reset = id

instance Hashable (Ref t) where
  hash = hash . toText . toName
  hashWithSalt s = (*s) . hash . toText . toName

class Referencable t where
    mkRef :: t -> Ref t
