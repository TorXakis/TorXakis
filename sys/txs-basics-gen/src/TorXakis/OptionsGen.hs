{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  OptionsGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a Generator for 'TorXakis.Options'.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.OptionsGen
( 
-- * Options Generator
  OptionsGen(..)
  -- dependencies, yet part of interface
, Options
)
where


import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import           GHC.Generics     (Generic)
import           Test.QuickCheck

import           TorXakis.PrettyPrint.TorXakis

-- | Definition of the name generator.
newtype OptionsGen = OptionsGen { -- | accessor to 'TorXakis.Options'
                                  unOptionsGen :: Options
                                }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)


instance Arbitrary OptionsGen
    where
        arbitrary = do
            ml <- arbitrary
            sh <- arbitrary
            return $ OptionsGen (Options ml sh)
