{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex.Regex
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for Regular Expressions.
-- We have chosen UTF-8 based on the needs of our current users
-- and the capabilities of the problem solvers.
-- UTF-8 has 256 characters from \x00 till \xFF
--
-- Note XSD and Posix Regex has two union operators (union operator and union of Char Group Parts)
-- Our implementation has like SMT Regex only one union operator.
-- Consequently, the Posix regular expression [A-Za-z_] will be printed as the equivalent expression [A-Z]|[a-z]|strLit '_'
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveAnyClass        #-}
module TorXakis.Regex.Regex
( -- * Regular Expression
  Regex (..)
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Set
import           GHC.Generics        (Generic)

-- | Regex: the private structure for regular expression 'Regex'
data Regex =  RegexEmpty
            | RegexCharLiteral Char
            | RegexRange Char Char                    -- invariant: lowerbound < upperbound
            | RegexConcat [Regex]                     -- invariant: length list is at least 2, RegexEmpty and RegexConcat are not contained, RegexStringLiteral do not appear consecutive
            | RegexUnion (Set Regex)                  -- invariant: number of elements is at least 2, elements are unique, RegexUnion is not contained
            | RegexLoop Regex Integer (Maybe Integer)
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)
