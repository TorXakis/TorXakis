{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ErrorGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a Generator for 'TorXakis.Error'.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.ErrorGen
( 
-- * ErrorGen Generator
  ErrorGen(..)
)
where


import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import qualified Data.Text as T
import           GHC.Generics     (Generic)
import           Test.QuickCheck

import           TorXakis.Error

-- | Definition of the name generator.
newtype ErrorGen = ErrorGen { -- | accessor to 'TorXakis.Error'
                              unErrorGen :: Error
                            }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)
    
instance Arbitrary ErrorGen
    where
        arbitrary = do
            s <- arbitrary :: Gen String
            return $ ErrorGen (Error (T.pack s))