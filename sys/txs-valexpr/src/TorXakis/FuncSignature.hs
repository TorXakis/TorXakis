{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncSignature
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Kerem Ispirli <kerem.ispirli@tno.nl>
--                Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the data structure for a Signature of a Function.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module TorXakis.FuncSignature
( -- * Function Signature
  FuncSignature (..)
) where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))
import           GHC.Generics         (Generic)

import           TorXakis.Name
import           TorXakis.Sort

-- | A generalized, type-safe reference.
data FuncSignature = FuncSignature { -- | The 'Name' of the function.
                                     funcName :: Name
                                     -- | The 'Sort's of the function arguments.
                                   , args :: [Sort]
                                     -- | The 'Sort' of the result that the function returns.
                                   , returnSort :: Sort
                                   }
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

instance Hashable FuncSignature where
    hashWithSalt s (FuncSignature n as r) = s `hashWithSalt`
                                            n `hashWithSalt`
                                            as `hashWithSalt`
                                            r
