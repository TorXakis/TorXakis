{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  -- ** Conversion List to Map By Name
, toMapByName
) where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hash, hashWithSalt))
import           Data.HashMap         (Map, fromList)

import           TorXakis.Name.Name   (Name, HasName, getName)

-- | A generalized, type-safe reference.
newtype RefByName t = RefByName { -- | This reference keeps a 'Name' that represents the entity.
                                  toName :: Name
                                }
    deriving (Eq, Ord, Show, Read, NFData, Data)

instance Hashable (RefByName t) where
    hashWithSalt s = (*s) . hash . toName

-- | Return 'Data.HashMap.Map' where the 'Name' of the element is taken as key
--   and the element itself is taken as value.
toMapByName :: HasName a => [a] -> Map (RefByName a) a
toMapByName = fromList . map (\e -> (RefByName (getName e),e))