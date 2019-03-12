{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  OperatorNameGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a Generator for 'TorXakis.OperatorName'.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.OperatorNameGen
( 
-- * Name Generator
  OperatorNameGen(..)
  -- dependencies, yet part of interface
, OperatorName
)
where


import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import qualified Data.Text as T
import           GHC.Generics     (Generic)
import           Test.QuickCheck

import           TorXakis.OperatorName

-- | Definition of the name generator.
newtype OperatorNameGen = OperatorNameGen { -- | accessor to 'TorXakis.OperatorName'
                                            unOperatorNameGen :: OperatorName}
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

nameChars :: String
nameChars = "=+-*/\\^<>|@&%"

genText :: Gen T.Text
genText = do
    s <- listOf1 (elements nameChars)
    return $ T.pack s

instance Arbitrary OperatorNameGen
    where
        arbitrary = do
            text <- genText
            case mkOperatorName text of
                    Right n -> return (OperatorNameGen n)
                    Left e  -> error $ "Error in OperatorNameGen: unexpected error " ++ show e
