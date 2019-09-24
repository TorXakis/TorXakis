{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex.Posix
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Posix conversions of regular expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Regex.Posix
( 
-- * Conversion to Posix
  toPosix
)
where
import           Data.Monoid            ((<>))
import           Data.Set
import           Data.Text

import           TorXakis.Regex
import           TorXakis.Regex.CharRepr

-- | encode Char to Posix
-- by escaping posix special/meta characters
encodeChar :: Char -> Text
encodeChar '\\' = "\\\\"
encodeChar '|'  = "\\|"
encodeChar '?'  = "\\?"
encodeChar '+'  = "\\+"
encodeChar '*'  = "\\*"
encodeChar '.'  = "\\."
encodeChar '{'  = "\\{"
encodeChar '}'  = "\\}"
encodeChar '['  = "\\["
encodeChar ']'  = "\\]"
encodeChar '('  = "\\("
encodeChar ')'  = "\\)"
encodeChar '^'  = "\\^"
encodeChar '$'  = "\\$"
encodeChar c    = Data.Text.singleton c

-- | transform Regular expression to Posix Text
toPosix :: Regex -> Text
toPosix t = pack "\\`" <> toPosixView (viewCharRepr t) <> pack "\\'"
    where
        toPosixView :: CharRepr -> Text
        toPosixView  RegexEmpty                 = Data.Text.pack "()"
        toPosixView (RegexCharLiteral c)        = encodeChar c
        toPosixView (RegexConcat cs)            =    Data.Text.singleton '('
                                                  <> intercalate (pack ")(") (Prelude.map toPosixView cs)
                                                  <> Data.Text.singleton ')'
        toPosixView (RegexUnion us)             = intercalate (Data.Text.singleton '|') (Prelude.map toPosixView (Data.Set.toList us))
        toPosixView (RegexLoop r l (Just u))    =    Data.Text.singleton '('
                                                  <> toPosixView r
                                                  <> Data.Text.singleton ')'
                                                  <> Data.Text.singleton '{'
                                                  <> pack (show l)
                                                  <> Data.Text.singleton ','
                                                  <> pack (show u)
                                                  <> Data.Text.singleton '}'
        toPosixView (RegexLoop r l Nothing)     =    Data.Text.singleton '('
                                                  <> toPosixView r
                                                  <> Data.Text.singleton ')'
                                                  <> Data.Text.singleton '{'
                                                  <> pack (show l)
                                                  <> Data.Text.singleton ','
                                                  <> Data.Text.singleton '}'
        toPosixView (RegexRange '-' ']')      =      Data.Text.pack "[].-\\-]"
        toPosixView (RegexRange '-' u)        =      Data.Text.pack "[.-"
                                                  <> Data.Text.singleton u
                                                  <> Data.Text.pack "-]"
        toPosixView (RegexRange ']' u)        =      Data.Text.pack "[]^-"
                                                  <> Data.Text.singleton u
                                                  <> Data.Text.singleton ']'
        toPosixView (RegexRange '^' u)        =      Data.Text.pack "[_-"
                                                  <> Data.Text.singleton u
                                                  <> Data.Text.pack "^]"
        toPosixView (RegexRange l ']')        =      Data.Text.pack "[]"
                                                  <> Data.Text.singleton l
                                                  <> Data.Text.pack "-\\]"
        toPosixView (RegexRange l u)          =      Data.Text.singleton '['
                                                  <> Data.Text.singleton l
                                                  <> Data.Text.singleton '-'
                                                  <> Data.Text.singleton u
                                                  <> Data.Text.singleton ']'
