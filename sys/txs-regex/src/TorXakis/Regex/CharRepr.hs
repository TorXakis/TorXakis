{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex.CharRepr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Representation for Regular Expressions using Characters.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveAnyClass        #-}
module TorXakis.Regex.CharRepr
( -- * Representation of Regular Expression using characters
  CharRepr (..)
, viewCharRepr
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.List
import qualified Data.Set
import           GHC.Generics        (Generic)

import qualified TorXakis.Regex.Regex

-- | CharRepr: a public view of regular expression 'Regex' based on characters
data CharRepr =  RegexEmpty
               | RegexCharLiteral Char
               | RegexRange Char Char
               | RegexConcat [CharRepr]
               | RegexUnion (Data.Set.Set CharRepr)
               | RegexLoop CharRepr Integer (Maybe Integer)
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | view on regular expression based on characters.
viewCharRepr :: TorXakis.Regex.Regex.Regex -> CharRepr
viewCharRepr  TorXakis.Regex.Regex.RegexEmpty           = RegexEmpty
viewCharRepr (TorXakis.Regex.Regex.RegexCharLiteral c)  = RegexCharLiteral c
viewCharRepr (TorXakis.Regex.Regex.RegexRange l u)      = RegexRange l u
viewCharRepr (TorXakis.Regex.Regex.RegexConcat rs)      = RegexConcat (Data.List.map viewCharRepr rs)
viewCharRepr (TorXakis.Regex.Regex.RegexUnion rs)       = RegexUnion (Data.Set.map viewCharRepr rs)
viewCharRepr (TorXakis.Regex.Regex.RegexLoop r l mu)    = RegexLoop (viewCharRepr r) l mu
