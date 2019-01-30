{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMLNameGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a Generator for 'TorXakis.XMLName'.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.XMLNameGen
( 
-- * XMLName Generator
  XMLNameGen(..)
)
where


import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import qualified Data.Text as T
import           GHC.Generics     (Generic)
import           Test.QuickCheck

import           TorXakis.XMLName

-- | Definition of the XMLName generator.
newtype XMLNameGen = XMLNameGen { -- | accessor to 'TorXakis.XMLName'
                            unXMLNameGen :: XMLName}
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

xmlNameStartChars :: String
xmlNameStartChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"

xmlNameChars :: String
xmlNameChars = xmlNameStartChars ++ "-0123456789"

instance Arbitrary XMLNameGen
    where
        arbitrary = do
            c <- elements xmlNameStartChars
            s <- listOf (elements xmlNameChars)
            case mkXMLName (T.pack (c:s)) of
                Right n -> return (XMLNameGen n)
                Left e  -> error $ "Error in XMLNameGen: unexpected error " ++ show e
