{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex.StringRepr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- View on Regular Expressions using strings.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveAnyClass        #-}
module TorXakis.Regex.StringRepr
( -- * Regular Expression Representation
  StringRepr (..)
, viewStringRepr
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.List
import qualified Data.Set
import qualified Data.Text
import           GHC.Generics        (Generic)

import qualified TorXakis.Regex.Regex

-- | StringRepr: view of regular expression 'TorXakis.Regex' based on strings
data StringRepr =  RegexStringLiteral Data.Text.Text
                 | RegexRange Char Char                    
                 | RegexConcat [StringRepr]                     
                 | RegexUnion (Data.Set.Set StringRepr)                  
                 | RegexLoop StringRepr Integer (Maybe Integer)
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | view Regex in String representation
viewStringRepr :: TorXakis.Regex.Regex.Regex -> StringRepr
viewStringRepr  TorXakis.Regex.Regex.RegexEmpty          = RegexStringLiteral Data.Text.empty
viewStringRepr (TorXakis.Regex.Regex.RegexCharLiteral c) = RegexStringLiteral $ Data.Text.singleton c
viewStringRepr (TorXakis.Regex.Regex.RegexRange l u)     = RegexRange l u
viewStringRepr (TorXakis.Regex.Regex.RegexConcat rs)     = case merge (Data.List.map viewStringRepr rs) of
                                                               []  -> error "merge of RegexConcat is unexpectedly empty"
                                                               [m] -> m
                                                               ms  -> RegexConcat ms
    where
        merge :: [StringRepr] -> [StringRepr]
        merge []  = []
        merge [x] = [x]
        merge ( RegexStringLiteral t1
              : RegexStringLiteral t2
              : xs)                     = merge (RegexStringLiteral (Data.Text.append t1 t2) : xs)
        merge ( x1 : x2 : xs )          = x1 : merge ( x2 : xs )
viewStringRepr (TorXakis.Regex.Regex.RegexUnion us)      = RegexUnion (Data.Set.map viewStringRepr us)
viewStringRepr (TorXakis.Regex.Regex.RegexLoop r l mu)   = let s = viewStringRepr r in
                                                                case (s, mu) of
                                                                    (RegexStringLiteral t, Just u) | l == u -> RegexStringLiteral $ Data.Text.concat (Data.List.genericReplicate l t)
                                                                    _                                       -> RegexLoop s l mu
