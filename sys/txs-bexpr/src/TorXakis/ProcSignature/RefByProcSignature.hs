{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RefByProcSignature
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Kerem Ispirli <kerem.ispirli@tno.nl>
--                Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides a reference by ProcSignature.
-----------------------------------------------------------------------------
module TorXakis.ProcSignature.RefByProcSignature
( -- * Reference By ProcSignature
  RefByProcSignature (..)
) where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))

import           TorXakis.ProcSignature.ProcSignature

-- | A reference by ProcSignature.
-- Note: no phantom type since it would create a cycle between BExpr and ProcDef.
-- (see https://wiki.haskell.org/Phantom_type)
newtype RefByProcSignature = 
            RefByProcSignature { -- | This reference keeps a 'ProcSignature' that represents the entity.
                                 toProcSignature :: ProcSignature
                               }
    deriving (Eq, Ord, Show, Read, NFData, Data)

instance Hashable RefByProcSignature where
    hashWithSalt s = hashWithSalt s . toProcSignature
