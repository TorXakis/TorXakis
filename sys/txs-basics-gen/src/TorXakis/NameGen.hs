{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NameGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a Generator for 'TorXakis.Name'.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.NameGen
( 
-- * Name Generator
  NameGen(..)
)
where


import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import qualified Data.Text as T
import           GHC.Generics     (Generic)
import           Test.QuickCheck

import           TorXakis.Name

-- | Definition of the name generator.
newtype NameGen = NameGen { -- | accessor to 'TorXakis.Name'
                            unNameGen :: Name}
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)
    
instance Arbitrary NameGen
    where
        arbitrary = do
            c <- arbitrary :: Gen Char
            s <- arbitrary :: Gen String
            case mkName (T.pack (c:s)) of
                Right n -> return (NameGen n)
                Left e  -> error $ "Error in NameGen: unexpected error " ++ show e
             
