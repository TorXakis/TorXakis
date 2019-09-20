{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.StringFromRegex
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> ESI (TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a Generator for string according to a 'TorXakis.Regex'.
-- Functionality is comparable to Xeger (see https://code.google.com/archive/p/xeger/)
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.StringFromRegex
( 
-- * Regex Generator
  stringFromRegex
  -- dependencies, yet part of interface
, Regex
)
where
import qualified Data.Char
import qualified Data.List
import qualified Data.Set
import qualified Data.Text
import           System.Random

import           TorXakis.Regex

-- | parameter needed to generate unbounded loops (like generated with * and +)
-- TODO: have really an exponentially decreasy likelihood of length
maxReplicate :: Integer
maxReplicate = 10         -- many, yet not too large to ensure fast generation

-- | generate a string that adheres to the given 'TorXakis.Regex'.
stringFromRegex :: Regex -> IO Data.Text.Text
stringFromRegex = stringFromRegexView . view

stringFromRegexView :: RegexView -> IO Data.Text.Text
stringFromRegexView (RegexStringLiteral t)      = return t
stringFromRegexView (RegexConcat rs)            = Data.Text.concat <$> mapM stringFromRegex rs
stringFromRegexView (RegexUnion s)              = let l = Data.Set.toList s in do
                                                    index <- randomRIO (0, length l -1)
                                                    stringFromRegex (l !! index)
stringFromRegexView (RegexLoop r l (Just u))    = do
                                                    count <- randomRIO (l,u)
                                                    Data.Text.concat <$> mapM stringFromRegex (Data.List.genericReplicate count r)
stringFromRegexView (RegexLoop r l Nothing)     = do
                                                    count <- randomRIO (0, maxReplicate)
                                                    Data.Text.concat <$> mapM stringFromRegex (Data.List.genericReplicate (l+count) r)
stringFromRegexView (RegexRange l u)            = Data.Text.singleton . Data.Char.chr <$> randomRIO (Data.Char.ord l, Data.Char.ord u)
