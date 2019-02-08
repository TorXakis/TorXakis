{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Error
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the basis error type for TorXakis.
-- Receivers of errors should depend only on the Error class.
-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
module TorXakis.Error
( 
  -- * Error
  Error(..)
) where

-- | An error in TorXakis.
data Error = forall a . Show a => Error a

-- | Error is instance of show
instance Show Error where
    show (Error a) = show a

instance Eq Error where
    a == b = show a == show b

instance Ord Error where
    compare a b = compare (show a) (show b)