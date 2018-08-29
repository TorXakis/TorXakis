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
-----------------------------------------------------------------------------
module TorXakis.Error
( 
  -- * Error
  Error (..)
) where
import Data.Text(Text)

-- | An error in TorXakis.
newtype Error = Error { -- | An error can be converted to 'Data.Text.Text'.
                            toText :: Text
                      }
                      deriving (Eq, Ord, Read, Show)