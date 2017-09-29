{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- |
-- Module      :  RegexRepr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Transcribe regular expression between different representations.
-- 
-- For more info on
--  * XSD representation see http://www.w3.org/TR/xmlschema11-2/#regexs
--  * Posix reprentation see https://wiki.haskell.org/Regular_expressions#regex-posix
--  * SMT reprentation see http://cvc4.cs.stanford.edu/wiki/Strings#Symbolic_Regular_Expression

-----------------------------------------------------------------------------
module RegexRepr
where

import           Data.Text     (Text)
import qualified Data.Text     as T
import           RegexAlex
import           RegexPosixHappy
import           RegexSMTHappy

-- | Transcribe regular expression in XSD to SmtLib representation.
xsd2smt :: Text -> Text
xsd2smt = regexSMTParser . regexLexer . T.unpack

-- | Transcribe regular expression in XSD to Posix representation.
xsd2posix :: Text -> Text
xsd2posix = regexPosixParser . regexLexer . T.unpack
