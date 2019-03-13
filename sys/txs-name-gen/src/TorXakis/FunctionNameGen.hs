{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FunctionNameGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a Generator for 'TorXakis.FunctionName'.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.FunctionNameGen
( 
-- * Name Generator
  FunctionNameGen(..)
  -- dependencies, yet part of interface
, FunctionName
)
where


import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import qualified Data.Text as T
import           GHC.Generics     (Generic)
import           Test.QuickCheck

import           TorXakis.Name
import           TorXakis.FunctionName

-- | Definition of the name generator.
newtype FunctionNameGen = FunctionNameGen { -- | accessor to 'TorXakis.FunctionName'
                                            unFunctionNameGen :: FunctionName}
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

nameStartChars :: String
nameStartChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"

nameChars :: String
nameChars = nameStartChars ++ "-0123456789"

genTextName :: Gen T.Text
genTextName = do
    c <- elements nameStartChars
    s <- listOf (elements nameChars)
    return $ T.pack (c:s)

operatorChars :: String
operatorChars = "=+-*/\\^<>|@&%"

genTextOperator :: Gen T.Text
genTextOperator = do
    s <- listOf1 (elements operatorChars)
    return $ T.pack s

genText :: Gen T.Text
genText = do
    text <- oneof [genTextName, genTextOperator]
    if isReservedToken text
        then discard
        else return text


instance Arbitrary FunctionNameGen
    where
        arbitrary = do
            text <- genText
            case mkFunctionName text of
                    Right n -> return (FunctionNameGen n)
                    Left e  -> error $ "Error in FunctionNameGen: unexpected error " ++ show e
