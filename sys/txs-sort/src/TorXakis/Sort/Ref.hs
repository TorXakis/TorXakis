{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
module TorXakis.Sort.Ref
( Ref (..)
, Referencable (..)
) where

import           Control.DeepSeq (NFData)
import           Data.Data       (Data)
import           Data.Hashable   (Hashable, hash, hashWithSalt)

import           TorXakis.Sort.Name (Name, HasName, getName, toText)

-- | A generalized, type-safe reference.
newtype Ref t = RefByName { -- | This reference keeps a 'Name' that represents the entity.
                            toName :: Name
                          }
                -- We will introduce RefBySignature for functions; that's why
                -- the constructor name is different than type name.
    deriving (Eq, Ord, Show, Read, NFData, Data)

instance Hashable (Ref t) where
    hash = hash . toText . toName
    hashWithSalt s = (*s) . hash . toText . toName

-- | Enables creating 'Ref's to entities in a common way.
class Referencable t where
    mkRef :: t -> Ref t
    default mkRef :: HasName t => t -> Ref t
    mkRef = RefByName . getName


