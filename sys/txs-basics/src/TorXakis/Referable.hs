{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Referable
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the referable class.
-- The referable class links a type to its reference type.
-----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies          #-}
module TorXakis.Referable
( -- * Referable
  Referable(..)
)
where
-- | A referable class
class Referable a c where
    type Ref a
    -- | Derive reference from provided definition.
    -- Functions and processes need a context to be able to derive the return sort of their bodies.
    toRef :: c -> a -> Ref a