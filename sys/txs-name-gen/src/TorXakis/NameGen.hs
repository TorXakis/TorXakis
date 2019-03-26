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
  -- dependencies, yet part of interface
, Name
)
where


import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import qualified Data.Text as T
import           GHC.Generics     (Generic)
import           Test.QuickCheck

import           TorXakis.Language
import           TorXakis.Name

-- | Definition of the name generator.
newtype NameGen = NameGen { -- | accessor to 'TorXakis.Name'
                            unNameGen :: Name}
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

nameStartChars :: String
nameStartChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"

nameChars :: String
nameChars = nameStartChars ++ "-0123456789"

genString :: Gen String
genString = do
    c <- elements nameStartChars
    s <- listOf (elements nameChars)
    return (c:s)

genName :: Gen NameGen
genName = do
    str <- genString
    if isTxsReserved str
        then discard
        else case mkName (T.pack str) of
                  Right n -> return (NameGen n)
                  Left e  -> error $ "Error in NameGen: unexpected error " ++ show e

genIsName :: Gen NameGen
genIsName = do
    str <- genString
    let isStr = TorXakis.Language.toString (txsNameIsConstructor str) in
        if isTxsReserved isStr
            then discard
            else case mkName (T.pack isStr) of
                      Right n -> return (NameGen n)
                      Left e  -> error $ "Error in NameGen: unexpected error " ++ show e

instance Arbitrary NameGen
    where
        arbitrary = oneof [genName, genIsName]
        
