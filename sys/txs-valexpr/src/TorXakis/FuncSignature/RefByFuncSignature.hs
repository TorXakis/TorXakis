{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RefByFuncSignature
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Kerem Ispirli <kerem.ispirli@tno.nl>
--                Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides a reference by FuncSignature.
-----------------------------------------------------------------------------
module TorXakis.FuncSignature.RefByFuncSignature
( -- * Reference By FuncSignature
  RefByFuncSignature (..)
) where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))

import           TorXakis.FuncSignature.FuncSignature

-- | A reference by FuncSignature.
-- Note: no phantom type since it would create a cycle between ValExpr and FuncDef.
-- (see https://wiki.haskell.org/Phantom_type)
newtype RefByFuncSignature = 
            RefByFuncSignature { -- | This reference keeps a 'FuncSignature' that represents the entity.
                                 toFuncSignature :: FuncSignature
                               }
    deriving (Eq, Ord, Show, Read, NFData, Data)

instance Hashable RefByFuncSignature where
    hashWithSalt s = hashWithSalt s . toFuncSignature
