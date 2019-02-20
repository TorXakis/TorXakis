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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module TorXakis.Referable
( -- * Referable
  Referable(..)
  -- * RefDerivable
, RefDerivable (..)
)
where
-- | A referable class
class Referable a where
    -- | The type of the reference
    type Ref a

-- | A referable class with a derivable reference.
class Referable a => RefDerivable a c where
    -- | Derive reference from provided definition.
    -- The provided definition might need a context to be able to derive the reference.
    -- For example, functions and processes need a context to be able to derive the return sort of their bodies.
    --
    -- Use 'TorXakis.EmptyContext' when no context is needed.
    -- One can't use 'Data.Void' since other contexts aren't derive from it.
    toRef :: c -> a -> Ref a