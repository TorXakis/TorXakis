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
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.Error
( 
  -- * Error
  Error(..)
  -- * Minimal Error Implementation
, MinError(MinError)
) where
import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import           Data.Text(Text)
import           GHC.Generics     (Generic)


-- | An error in TorXakis.
class Error a where
    -- | An error can be converted to 'Data.Text.Text'.
    toText :: a -> Text

-- | A minimal error.
--   Instance of 'Error'
newtype MinError = 
    MinError {
          -- | A minimal error just contains a 'Data.Text.Text'.
          _toText :: Text
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Error MinError where
    toText = _toText